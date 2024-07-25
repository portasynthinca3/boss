//! Basic BEAM interpreter

use core::iter;

use alloc::vec;
use alloc::{borrow::ToOwned, boxed::Box, rc::Rc, vec::Vec};

use crate::vm::scheduler::{ExecuteStatus, TransferAgent};

use super::{module::{Instruction, Module, Opcode, Operand}, scheduler::{CommonState, Execute, ExecuteMake}, state::{LocalAtomRef, LocalContext}, term::LocalTerm};

#[derive(Clone, Debug)]
pub struct InstructionPtr {
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
    fn new<'a>(entry: InstructionPtr, args: &'a [LocalTerm]) -> BeamState {
        BeamState {
            x: Vec::from(args),
            y: Vec::new(),
            stop: 0,
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
                    self.x.extend(iter::repeat(LocalTerm::nil()).take(extend_by));
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

    /// Sets up a stack frame
    fn setup_frame(&mut self, y_regs: usize, cp: Option<InstructionPtr>) {
        self.y.push(YRegister::StkFrame(cp, self.stop));
        self.y.extend(iter::repeat(YRegister::Term(LocalTerm::nil())).take(y_regs));
        self.stop += 1;
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

        match (insn.opcode, &insn.operands) {
            (Opcode::Label | Opcode::Line, _) => Ok(()),

            (Opcode::CallExt, [Some(Operand::Number(arity)), Some(Operand::Number(dest)), ..]) => {
                let arity: usize = arity.try_into().map_err(|_| Terminate::BadInsn)?;
                let dest: usize = dest.try_into().map_err(|_| Terminate::BadInsn)?;
                let (module, fun, import_arity) = self.state.ip.module.imports[dest].clone();
                if import_arity != arity { bad_insn!(); }
                if module != erlang_atom { bad_insn!(); } // TODO: actual calls :^)
                self.state.x.truncate(arity);
                self.bif(fun, context)
            },

            (Opcode::Allocate, [Some(Operand::Number(stack)), Some(Operand::Number(live)), ..]) => {
                // deallocate all terms past X[live] and make a stack frame
                let stack = stack.try_into().map_err(|_| Terminate::BadInsn)?;
                let live = live.try_into().map_err(|_| Terminate::BadInsn)?;
                self.state.x.truncate(live);
                self.state.setup_frame(stack, self.state.cp.clone());
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

            (Opcode::Deallocate, [Some(Operand::Number(_)), ..]) => {
                let YRegister::StkFrame(ref cp, stop) = self.state.y[self.state.stop - 1] else {
                    panic!("corrupted stack frame");
                };
                self.state.cp = cp.clone();
                self.state.stop = stop;
                Ok(())
            },

            (Opcode::Return, [..]) => {
                if let Some(ref cp) = self.state.cp {
                    self.state.ip = cp.clone();
                    Ok(())
                } else {
                    Err(Terminate::Normal)
                }
            },

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

            (Opcode::IsEqExact, [Some(Operand::Label(fail)), Some(left), Some(right), ..]) => {
                let left = self.state.get_operand(left)?;
                let right = self.state.get_operand(right)?;
                if left != right {
                    jump!(self, *fail);
                }
                Ok(())
            },

            (Opcode::IsTuple, [Some(Operand::Label(fail)), Some(val), ..]) => {
                let val = self.state.get_operand(val)?;
                let LocalTerm::Tuple(_) = val else { jump!(self, *fail) };
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
                        let ref left = self.state.get_operand(left)?;
                        let Some(value) = src.0.get(left) else { jump!(self, *fail) };
                        self.state.assign_to_operand(right, value.clone())?;
                    }
                    Ok(())
                } else { jump!(self, *fail) }
            },

            (Opcode::IsTaggedTuple, [Some(Operand::Label(fail)), Some(src), Some(Operand::Number(arity)), Some(tag), ..]) => {
                let src = self.state.get_operand(src)?;
                let tag = self.state.get_operand(tag)?;
                let arity: usize = arity.try_into().map_err(|_| Terminate::BadInsn)?;
                let LocalTerm::Tuple(src) = src else { jump!(self, *fail) };
                if src.len() != arity { jump!(self, *fail) };
                if src.get(0) != Some(&tag) { jump!(self, *fail) };
                Ok(())
            },

            (Opcode::Badmatch, [Some(arg), ..]) => { // TODO: not ignore arg
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

            // receive markers enhance performance if implemented, but not required
            (Opcode::RecvMarkerBind | Opcode::RecvMarkerClear
            | Opcode::RecvMarkerReserve | Opcode::RecvMarkerUse, _) => Ok(()),

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
                    self.common.status = ExecuteStatus::Exited;
                    return;
                },
                Err(Terminate::BadInsn) => {
                    #[cfg(feature = "trace-beam")]
                    {
                        log::error!("invalid combination of opcode and operands or opcode not implemented:");
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
                module,
                instruction: fun_instruction
            }, init.3),
        })
    }
}
