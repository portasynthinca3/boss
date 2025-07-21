//! Basic BEAM interpreter

use core::iter;
use alloc::vec;
use alloc::{boxed::Box, rc::Rc, vec::Vec};

use crate::vm::scheduler::{ExecuteStatus, TransferAgent};
use super::{module::{Instruction, Module, Opcode, Operand}, scheduler::{CommonState, Execute, ExecuteMake}, state::{LocalAtomRef, LocalContext}, term::LocalTerm};

#[derive(Clone, Debug)]
pub struct InstructionPtr {
    application: LocalAtomRef,
    module: Rc<Module>,
    instruction: usize,
}
impl InstructionPtr {
    /// Dumps the instruction and the instructions around it
    pub fn log_context(&self) {
        let low = (self.instruction - 2).max(0);
        let high = (self.instruction + 3).min(self.module.instructions.len());
        for i in low..high {
            if i == self.instruction {
                log::error!("\x1b[31m  >>> \x1b[38;5;238m{:#?}", self.module.instructions[i]);
            } else {
                log::error!("\x1b[38;5;238m{: >4}: {:#?}", i, self.module.instructions[i]);
            }
        }
    }
}

/// Values stored on the stack (in the Y "registers")
#[derive(Clone, Debug)]
pub enum YRegister {    
    /// Register storing a term
    Term(LocalTerm),
    /// Register storing a stack frame (continuation pointer and top of stack)
    StkFrame(Option<InstructionPtr>, usize),
}

/// Architectural state of the BEAM virtual machine
#[derive(Clone, Debug)]
struct BeamState {
    /// X registers whose values are not preserved across function calls
    x: Vec<LocalTerm>,
    /// Y "registers" form a stack divided into stack frames. Values in these
    /// registers are preserved across function calls
    y: Vec<YRegister>,
    /// The top of the stack, i.e. the lowest Y register that is visible to the
    /// current function
    stop: usize,
    /// Message counter, used in a receive loop
    loop_rec_ctr: usize,
    /// Instruction pointer
    ip: InstructionPtr,
    /// Continuation (return) pointer
    cp: Option<InstructionPtr>,
}

/// Instruction interpretation should stop and something else should be done
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Terminate {
    /// Undefined application error
    NoApp,
    /// Undefined function error
    NoFun,
    /// Undefined module error
    NoMod,
    /// Match error
    Badmatch,
    /// Bad instruction
    BadInsn,
    /// Enter wait state, wait until a message is received
    EnterWait,
    /// Normal termination of process
    Normal,
}

impl BeamState {
    /// Creates a new state that starts execution at the specified entry point
    fn new(entry: InstructionPtr, args: &[LocalTerm]) -> BeamState {
        BeamState {
            x: Vec::from(args),
            y: vec![YRegister::StkFrame(None, 0)],
            stop: 1,
            loop_rec_ctr: 0,
            ip: entry,
            cp: None,
        }
    }

    /// Takes the value of an [Operand]. An operand may depend on the state of
    /// the virtual machine, e.g. if it's a register.
    fn get_operand(&self, operand: &Operand) -> Result<LocalTerm, Terminate> {
        match operand {
            Operand::Nil => Ok(LocalTerm::List(Box::new([]), None)),
            Operand::Atom(atom) => Ok(LocalTerm::Atom(atom.clone())),
            Operand::Literal(lit) => Ok(lit.clone()),
            Operand::Number(num) => Ok(LocalTerm::Integer(num.clone())),
            Operand::XReg(x) => {
                match self.x.get(*x) {
                    Some(term) => Ok(term.clone()),
                    None => Err(Terminate::BadInsn),
                }
            },
            Operand::YReg(y) => {
                match self.y.get(self.stop + *y) {
                    Some(YRegister::Term(term)) => Ok(term.clone()),
                    _ => Err(Terminate::BadInsn),
                }
            },
            _ => Err(Terminate::BadInsn),
        }
    }

    /// Assigns `value` to the supplied [Operand].
    fn assign_to_operand(&mut self, operand: &Operand, value: LocalTerm) -> Result<(), Terminate> {
        match operand {
            Operand::XReg(x) => {
                let x = *x;
                if x >= self.x.len() {
                    let extend_by = x - self.x.len() + 1;
                    self.x.extend(iter::repeat_n(LocalTerm::nil(), extend_by));
                }
                self.x[x] = value;
                Ok(())
            },
            Operand::YReg(y) => {
                let y = *y + self.stop;
                if y >= self.y.len() {
                    return Err(Terminate::BadInsn);
                }
                self.y[y] = YRegister::Term(value);
                Ok(())
            },
            _ => Err(Terminate::BadInsn),
        }
    }
}

/// Interprets instructions one by one without any optimizations
pub struct BeamInterpreter {
    common: CommonState,
    state: BeamState,
}

// convenience macros
macro_rules! jump {
    ($self:ident, $label:expr) => {
        {
            $self.state.ip.instruction = $self.state.ip.module.labels[$label];
            return Ok(())
        }
    };
}
macro_rules! bad_insn {
    () => {
        return Err(Terminate::BadInsn)
    };
}
macro_rules! atom_match {
    ($a:ident, $ctx:expr, $at:expr) => {
        $ctx.atom_table.get_or_make_atom($at) == $a
    }
}

impl BeamInterpreter {
    /// Runs a single instruction, transforming the state in the process.
    fn run_insn(&mut self, context: &mut LocalContext, insn: &Instruction) -> Result<(), Terminate> {
        let erlang_atom = context.atom_table.get_or_make_atom("erlang");
        let true_atom = context.atom_table.get_or_make_atom("true");
        let false_atom = context.atom_table.get_or_make_atom("false");

        // here goes that ginormous match statement that's in every interpreter
        // of mine. are there any better ways to do this?
        match (insn.opcode, &insn.operands) {
            (Opcode::Label | Opcode::Line, _) => Ok(()),

            (Opcode::FuncInfo, _) => {
                Err(Terminate::Badmatch)
            },

            // ----====----
            // flow control
            // ----====----

            (Opcode::CallExt, [Some(Operand::Number(arity)), Some(Operand::Number(dest)), ..]) => {
                let arity: usize = arity.try_into().map_err(|_| Terminate::BadInsn)?;
                let dest: usize = dest.try_into().map_err(|_| Terminate::BadInsn)?;
                let (module, fun, import_arity) = self.state.ip.module.imports[dest].clone();
                if import_arity != arity { bad_insn!(); }
                self.state.x.truncate(arity);
                if module == erlang_atom {
                    self.bif(fun, context)
                } else {
                    self.state.cp = Some(self.state.ip.clone());
                    // TODO: far calls
                    let target_app = context.applications.get(&self.state.ip.application).ok_or(Terminate::NoApp)?;
                    let target_module = Rc::clone(target_app.modules.get(&module)
                        .ok_or(Terminate::NoMod)?.as_ref()
                        .ok_or(Terminate::NoMod)?);
                    let target_label = *target_module.exports.get(&(fun, arity)).ok_or(Terminate::NoFun)?;
                    let target_insn = *target_module.labels.get(target_label).ok_or(Terminate::NoFun)?;
                    self.state.y.push(YRegister::StkFrame(self.state.cp.clone(), self.state.stop));
                    self.state.stop = self.state.y.len();
                    self.state.ip = InstructionPtr {
                        application: self.state.ip.application.clone(),
                        module: target_module,
                        instruction: target_insn,
                    };
                    Ok(())
                }
            },

            // literally could not figure out the difference between these two
            (Opcode::Jump, [Some(Operand::Label(label)), ..]) |
            (Opcode::CallOnly, [Some(Operand::Number(_)), Some(Operand::Label(label)), ..]) => {
                jump!(self, *label);
            },

            (Opcode::Return, [..]) => {
                if let Some(ref cp) = self.state.cp {
                    self.state.ip = cp.clone();
                    Ok(())
                } else {
                    Err(Terminate::Normal)
                }
            },

            (Opcode::IntCodeEnd, [..]) => bad_insn!(),

            // -----=======-----
            // memory management
            // -----=======-----

            (Opcode::Allocate, [Some(Operand::Number(stack)), Some(Operand::Number(live)), ..]) => {
                // deallocate all terms past X[live] and make a stack frame
                let stack = stack.try_into().map_err(|_| Terminate::BadInsn)?;
                let live = live.try_into().map_err(|_| Terminate::BadInsn)?;
                self.state.x.truncate(live);
                self.state.y.extend(iter::repeat_n(YRegister::Term(LocalTerm::nil()), stack));
                Ok(())
            },

            (Opcode::Deallocate, [Some(Operand::Number(_)), ..]) => {
                // the given argument is ignored for safety
                let Some(YRegister::StkFrame(ref cp, stop)) = self.state.y.get(self.state.stop - 1) else {
                    log::error!("BEAM stack (stop={})", self.state.stop);
                    for (i, elem) in self.state.y.iter().enumerate() {
                        log::error!("{i}: {elem:?}");
                    }
                    panic!("corrupted BEAM stack frame"); // TODO: maybe do not crash the entire system
                };
                let old_stop = self.state.stop;
                self.state.cp.clone_from(cp);
                self.state.stop = *stop;
                self.state.y.truncate(old_stop - 1);
                Ok(())
            },

            (Opcode::TestHeap, [Some(_), Some(Operand::Number(live)), ..]) => {
                // The first register specifies the number in words that need to
                // be allocated. This hint is meaningless to us because the way
                // that we lay out data differs from how BEAM does so.
                let live = live.try_into().map_err(|_| Terminate::BadInsn)?;
                self.state.x.truncate(live);
                Ok(())
            },

            (Opcode::Trim, [Some(Operand::Number(_how_much)), Some(Operand::Number(remaining)), ..]) => {
                let remaining: usize = remaining.try_into().map_err(|_| Terminate::BadInsn)?;
                self.state.y.truncate(self.state.stop + remaining);
                Ok(())
            },

            // all registers are already initialized with nil
            (Opcode::InitYregs, [Some(Operand::List(_regs)), ..]) => Ok(()),

            // ---===---
            // messaging
            // ---===---

            (Opcode::Send, [..]) => {
                if self.state.x.len() < 2 { bad_insn!(); }
                let (LocalTerm::Pid(receiver) | LocalTerm::Port(receiver)) = self.state.x[0] else { bad_insn!() };
                let complete_msg = LocalTerm::Tuple(vec![LocalTerm::Pid(self.common.id), self.state.x[1].clone()]);
                context.messenger.as_mut()
                    .expect("transfer agent not initialized")
                    .route_message(receiver, complete_msg, 16);
                Ok(())
            },

            (Opcode::RemoveMessage, [..]) => {
                self.common.mailbox.remove(self.state.loop_rec_ctr);
                self.state.loop_rec_ctr = 0;
                Ok(())
            },

            (Opcode::LoopRec, [Some(Operand::Label(fail)), Some(dest), ..]) => {
                let message = self.common.mailbox.get(self.state.loop_rec_ctr);
                let Some(message) = message else { jump!(self, *fail) };
                self.state.assign_to_operand(dest, message.clone())?;
                Ok(())
            },

            (Opcode::LoopRecEnd, [Some(Operand::Label(next)), ..]) => {
                self.state.loop_rec_ctr += 1;
                jump!(self, *next);
            },

            (Opcode::Wait, [Some(Operand::Label(cont)), ..]) => {
                self.state.loop_rec_ctr = 0;
                self.state.ip.instruction = self.state.ip.module.labels[*cont];
                Err(Terminate::EnterWait)
            },

            (Opcode::WaitTimeout, [Some(Operand::Label(cont)), Some(time), ..]) => {
                self.state.loop_rec_ctr = 0;
                self.state.ip.instruction = self.state.ip.module.labels[*cont];
                Err(Terminate::EnterWait)
            },

            (Opcode::Timeout, [..]) => {
                todo!();
            },

            // receive markers enhance performance if implemented, but not required
            (Opcode::RecvMarkerBind | Opcode::RecvMarkerClear
            | Opcode::RecvMarkerReserve | Opcode::RecvMarkerUse, _) => Ok(()),

            // ----===----
            // term guards
            // ----===----

            (Opcode::IsTuple, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::Tuple(_) = val else { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::IsAtom, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::Atom(_) = val else { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::IsBitstr, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::BitString(_, _) = val else { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::IsBinary, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::BitString(len, _) = val else { jump!(self, *fail) };
                if len % 8 != 0 { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::IsBoolean, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::Atom(atom) = val else { jump!(self, *fail) };
                if atom != true_atom && atom != false_atom { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::IsInteger, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::Integer(_) = val else { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::IsPid, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::Pid(_) = val else { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::IsPort, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::Port(_) = val else { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::IsReference, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::Reference(_) = val else { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::IsList, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::List(_, _) = val else { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::IsNonemptyList, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::List(list, _) = val else { jump!(self, *fail) };
                if list.is_empty() { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::IsNil, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::List(list, _) = val else { jump!(self, *fail) };
                if !list.is_empty() { jump!(self, *fail) };
                Ok(())
            },

            // -----=====-----
            // term operations
            // -----=====-----

            (Opcode::IsEqExact, [Some(Operand::Label(fail)), Some(left), Some(right), ..]) => {
                let left = self.state.get_operand(left)?;
                let right = self.state.get_operand(right)?;
                if left != right {
                    jump!(self, *fail);
                }
                Ok(())
            },

            (Opcode::TestArity, [Some(Operand::Label(fail)), Some(val), Some(Operand::Number(arity)), ..]) => {
                let arity: usize = arity.try_into().map_err(|_| Terminate::BadInsn)?;
                let val = self.state.get_operand(val)?;
                let LocalTerm::Tuple(tuple) = val else { bad_insn!() };
                if tuple.len() != arity {
                    jump!(self, *fail);
                }
                Ok(())
            },

            (Opcode::Move, [Some(src), Some(dest), ..]) => {
                let src = self.state.get_operand(src)?;
                self.state.assign_to_operand(dest, src)?;
                Ok(())
            },

            (Opcode::GetTupleElement, [Some(src), Some(Operand::Number(index)), Some(dest), ..]) => {
                let index: usize = index.try_into().map_err(|_| Terminate::BadInsn)?;
                let LocalTerm::Tuple(src) = self.state.get_operand(src)? else { bad_insn!() };
                let element = src.get(index).ok_or(Terminate::BadInsn)?;
                self.state.assign_to_operand(dest, element.clone())?;
                Ok(())
            },

            (Opcode::IsTaggedTuple, [Some(Operand::Label(fail)), Some(src), Some(Operand::Number(arity)), Some(tag), ..]) => {
                let src = self.state.get_operand(src)?;
                let tag = self.state.get_operand(tag)?;
                let arity: usize = arity.try_into().map_err(|_| Terminate::BadInsn)?;
                let LocalTerm::Tuple(src) = src else { jump!(self, *fail) };
                if src.len() != arity { jump!(self, *fail) };
                if src.first() != Some(&tag) { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::Badmatch, [Some(_arg), ..]) => { // TODO: not ignore arg
                Err(Terminate::Badmatch)
            },

            (Opcode::PutTuple2, [Some(dest), Some(Operand::List(tuple)), ..]) => {
                let mut out = Vec::with_capacity(tuple.len());
                for element in tuple.iter() {
                    out.push(self.state.get_operand(element)?);
                }
                self.state.assign_to_operand(dest, LocalTerm::Tuple(out))?;
                Ok(())
            },

            // -==-
            // maps
            // -==-

            (Opcode::IsMap, [Some(Operand::Label(fail)), Some(arg), ..]) => {
                let arg = self.state.get_operand(arg);
                match arg {
                    Ok(LocalTerm::Map(_)) => Ok(()),
                    _ => jump!(self, *fail),
                }
            },
            
            (Opcode::GetMapElements, [Some(Operand::Label(fail)), Some(src), Some(Operand::List(spec)), ..]) => {
                let src = self.state.get_operand(src)?;
                if let LocalTerm::Map(src) = src {
                    // `spec` is encoded as a sequence of KV pairs
                    // the value corresponding to K is fetched and put into V
                    let (chunks, []) = spec.as_chunks::<2>() else { bad_insn!() };
                    for [left, right] in chunks {
                        let left = self.state.get_operand(left)?;
                        let Some(value) = src.0.get(&left) else { jump!(self, *fail) };
                        self.state.assign_to_operand(right, value.clone())?;
                    }
                    Ok(())
                } else { jump!(self, *fail) }
            },

            (_, _) => {
                Err(Terminate::BadInsn)
            },
        }
    }

    /// Executes a Built-In Function
    fn bif(&mut self, fun: LocalAtomRef, context: &mut LocalContext) -> Result<(), Terminate> {
        match fun {
            a if atom_match!(a, context, "make_ref") => {
                self.state.x.push(context.make_ref());
                Ok(())
            },
            _ => Err(Terminate::BadInsn),
        }
    }
}

/// The errors of [BeamInterpreter::new]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::enum_variant_names)]
pub enum BeamInterpreterMakeError {
    /// Application not found in local context
    NoApp,
    /// Module not found in application
    NoMod,
    /// Function/arity not found in module
    NoFun,
}

impl Execute for BeamInterpreter {
    fn get_common_state(&self) -> &CommonState { &self.common }
    fn get_common_state_mut(&mut self) -> &mut CommonState { &mut self.common }

    fn run_for(&mut self, context: &mut LocalContext, mut reductions: isize) {
        loop {
            // fetch instruction
            #[cfg(feature = "trace-beam")]
            let ip = self.state.ip.clone();
            let instruction = &self.state.ip.module.instructions[self.state.ip.instruction].clone();
            #[cfg(feature = "trace-beam")]
            log::trace!("{instruction:#?}");
            self.state.ip.instruction += 1;

            // execute instruction
            let result = self.run_insn(context, instruction);
            match result {
                Ok(()) => (),
                Err(Terminate::Normal) => {
                    #[cfg(feature = "trace-beam")]
                    log::trace!("process {:?} exited normally", self.common.id);
                    self.common.status = ExecuteStatus::Exited;
                    return;
                },
                Err(Terminate::EnterWait) => {
                    self.common.status = ExecuteStatus::Waiting;
                    return;
                },
                Err(Terminate::Badmatch) => {
                    #[cfg(feature = "trace-beam")]
                    {
                        log::error!("badmatch exception");
                        ip.log_context();
                    }
                    self.common.status = ExecuteStatus::Exited;
                    return;
                },
                Err(Terminate::BadInsn) => {
                    #[cfg(feature = "trace-beam")]
                    {
                        log::error!("invalid combination of opcode and operands or opcode not implemented");
                        ip.log_context();
                    }
                    self.common.status = ExecuteStatus::Exited;
                    return;
                },
                Err(Terminate::NoApp) => {
                    #[cfg(feature = "trace-beam")]
                    {
                        log::error!("application not found");
                        ip.log_context();
                    }
                    self.common.status = ExecuteStatus::Exited;
                    return;
                },
                Err(Terminate::NoMod) => {
                    #[cfg(feature = "trace-beam")]
                    {
                        log::error!("module not found in application");
                        ip.log_context();
                    }
                    self.common.status = ExecuteStatus::Exited;
                    return;
                },
                Err(Terminate::NoFun) => {
                    #[cfg(feature = "trace-beam")]
                    {
                        log::error!("function not found in module");
                        ip.log_context();
                    }
                    self.common.status = ExecuteStatus::Exited;
                    return;
                },
            }

            // determine if we're ought to finish executing
            reductions -= 1;
            if reductions <= 0 {
                break;
            }
        }
    }
}

impl<'i> ExecuteMake<'i> for BeamInterpreter {
    const IS_PORT: bool = false;
    type Init = (LocalAtomRef, LocalAtomRef, LocalAtomRef, &'i [LocalTerm]);
    type Error = BeamInterpreterMakeError;
    fn new(common: CommonState, init: &'i Self::Init, context: &LocalContext) -> Result<Self, Self::Error> {
        use BeamInterpreterMakeError::*;
        let app = context.applications.get(&init.0).ok_or(NoApp)?;
        let module = Rc::clone(app.modules.get(&init.1).ok_or(NoMod)?.as_ref().ok_or(NoMod)?);
        let fun_label = *module.exports.get(&(init.2.clone(), init.3.len())).ok_or(NoFun)?;
        let fun_instruction = module.labels[fun_label];
        Ok(Self {
            common,
            state: BeamState::new(InstructionPtr {
                application: init.0.clone(),
                module,
                instruction: fun_instruction
            }, init.3),
        })
    }
}
