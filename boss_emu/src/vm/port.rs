use alloc::vec;

use super::{scheduler::{CommonState, Eid, Execute, ExecuteMake, ExecuteStatus, TransferAgent}, state::LocalContext, term::LocalTerm};

/// Abstraction for ports. The corresponding abstraction in Erlang is
/// implemented in `base/gen_port.erl`.
trait Port {
    fn get_common_state(&self) -> &CommonState;
    fn get_common_state_mut(&mut self) -> &mut CommonState;
    fn handle_call(&mut self, context: &mut LocalContext, request: &LocalTerm, from: Eid, token: Option<&LocalTerm>) -> Option<LocalTerm>;
}

impl<T: Port + 'static> Execute for T {
    fn get_common_state(&self) -> &CommonState {
        self.get_common_state()
    }
    fn get_common_state_mut(&mut self) -> &mut CommonState {
        self.get_common_state_mut()
    }
    fn run_for(&mut self, context: &mut LocalContext, mut reductions: isize) {
        let notoken_atom: LocalTerm = context.atom_table.get_or_make_atom("notoken").into();

        loop {
            // receive message
            let message = self.get_common_state_mut().mailbox.pop_front();
            let Some(message) = message else {
                self.get_common_state_mut().status = ExecuteStatus::Waiting;
                return;
            };

            // deconstruct message
            let Ok([LocalTerm::Pid(from) | LocalTerm::Port(from), message]) = message.get_tuple::<2>() else { continue; };
            let Ok([
                conversation @ LocalTerm::Reference(_),
                token,
                request,
            ]) = message.get_tagged_tuple::<3>("$gen_port_call", context) else { continue; };
            let token = match token {
                token @ LocalTerm::Reference(_) => Some(token),
                atom if atom == &notoken_atom => None,
                _ => continue,
            };

            // handle message
            let reply = self.handle_call(context, request, *from, token);

            // send reply
            if let Some(reply) = reply {
                let reply = LocalTerm::Tuple(vec![conversation.clone(), reply]);
                let reply = LocalTerm::Tuple(vec![LocalTerm::Port(self.get_common_state().id), reply]);
                context.messenger.as_mut().unwrap().route_message(*from, reply, 16);
            }

            reductions -= 10;
            if reductions <= 0 {
                return;
            }
        }
    }
}

pub struct LogPort {
    common: CommonState,
    token: Option<LocalTerm>,
}

impl Port for LogPort {
    fn get_common_state(&self) -> &CommonState { &self.common }
    fn get_common_state_mut(&mut self) -> &mut CommonState { &mut self.common }

    fn handle_call(&mut self, context: &mut LocalContext, request: &LocalTerm, from: Eid, token: Option<&LocalTerm>) -> Option<LocalTerm> {
        // requests
        let mint_token_atom: LocalTerm = context.atom_table.get_or_make_atom("mint_token").into();
        let write_atom: LocalTerm = context.atom_table.get_or_make_atom("write").into();
        // conditions
        let ok_atom: LocalTerm = context.atom_table.get_or_make_atom("ok").into();
        let error_atom: LocalTerm = context.atom_table.get_or_make_atom("error").into();
        // errors
        let badarg_atom: LocalTerm = context.atom_table.get_or_make_atom("badarg").into();
        let access_atom: LocalTerm = context.atom_table.get_or_make_atom("access").into();

        Some(match request {
            r if *r == mint_token_atom && self.token.is_none() => {
                let token = context.make_ref();
                self.token = Some(token.clone());
                LocalTerm::Tuple(vec![ok_atom.clone(), token])
            },
            r if *r == mint_token_atom => {
                LocalTerm::Tuple(vec![error_atom.clone(), access_atom.clone()])
            },
            tup @ LocalTerm::Tuple(_) if
            let Ok([req, LocalTerm::BitString(len, data)]) = tup.get_tuple::<2>()
            && req == &write_atom
            && len % 8 == 0
            && let Ok(message) = core::str::from_utf8(data.as_slice()) => {
                if token.is_some() && token == self.token.as_ref() {
                    log::info!("{from:?} says: {message}");
                    ok_atom.clone()
                } else {
                    LocalTerm::Tuple(vec![error_atom.clone(), access_atom.clone()])
                }
            },
            r if *r == write_atom => {
                LocalTerm::Tuple(vec![error_atom.clone(), access_atom.clone()])
            },
            _ => {
                LocalTerm::Tuple(vec![error_atom.clone(), badarg_atom.clone()])
            },
        })
    }
}

impl<'i> ExecuteMake<'i> for LogPort {
    const IS_PORT: bool = true;
    type Init = ();
    type Error = !;
    fn new(common: CommonState, _init: &'i Self::Init, _ctx: &LocalContext) -> Result<Self, Self::Error> {
        Ok(LogPort { common, token: None })
    }
}

