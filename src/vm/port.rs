use alloc::vec;

use super::{scheduler::{CommonState, Execute, ExecuteMake, ExecuteStatus, TransferAgent}, state::LocalContext, term::LocalTerm};

pub struct LogPort {
    common: CommonState,
    token: Option<LocalTerm>,
}

impl Execute for LogPort {
    fn get_common_state(&self) -> &CommonState { &self.common }
    fn get_common_state_mut(&mut self) -> &mut CommonState { &mut self.common }
    fn get_status(&self) -> ExecuteStatus { self.common.status }

    fn run_for(&mut self, context: &mut LocalContext, mut reductions: isize) {
        let ok_atom: LocalTerm = context.atom_table.get_or_make_atom("ok").into();
        let error_atom: LocalTerm = context.atom_table.get_or_make_atom("error").into();
        let instantiated_atom: LocalTerm = context.atom_table.get_or_make_atom("instantiated").into();
        let badarg_atom: LocalTerm = context.atom_table.get_or_make_atom("badarg").into();
        let token_atom: LocalTerm = context.atom_table.get_or_make_atom("token").into();
        let mint_token_atom: LocalTerm = context.atom_table.get_or_make_atom("mint_token").into();
        let write_atom: LocalTerm = context.atom_table.get_or_make_atom("write").into();

        loop {
            // receive message
            let message = self.common.mailbox.pop_front();
            let Some(message) = message else {
                self.common.status = ExecuteStatus::Waiting;
                return;
            };

            // deconstruct message
            let LocalTerm::Tuple(message) = message else { continue };
            let Some(sender_term @ LocalTerm::Pid(sender)) = message.first() else { continue };
            let Some(message) = message.get(1) else { continue };
            let LocalTerm::Tuple(message) = message else { continue };
            let Some(conversation) = message.first() else { continue };
            let LocalTerm::Reference(_) = conversation else { continue };
            let Some(request) = message.get(1) else { continue };
            let Some(token) = message.get(2) else { continue };
            let Some(LocalTerm::List(args, None)) = message.get(3) else { continue };

            // process request
            let reply = match request {
                r if *r == mint_token_atom && self.token.is_none() => {
                    let token = context.make_ref();
                    self.token = Some(token.clone());
                    LocalTerm::Tuple(vec![ok_atom.clone(), token])
                },
                r if *r == mint_token_atom => {
                    LocalTerm::Tuple(vec![error_atom.clone(), instantiated_atom.clone()])
                },
                r if *r == write_atom && Some(token) == self.token.as_ref() => {
                    if let Some(LocalTerm::BitString(_, message)) = args.first()
                    && let Ok(message) = core::str::from_utf8(message) {
                        log::info!("process {sender_term:?} says: {message}");
                        ok_atom.clone()
                    } else {
                        LocalTerm::Tuple(vec![error_atom.clone(), badarg_atom.clone()])
                    }
                },
                r if *r == write_atom => {
                    LocalTerm::Tuple(vec![error_atom.clone(), token_atom.clone()])
                },
                _ => {
                    LocalTerm::Tuple(vec![error_atom.clone(), badarg_atom.clone()])
                },
            };

            let self_pid = LocalTerm::Port(self.common.id);
            let message = LocalTerm::Tuple(vec![conversation.clone(), reply]);
            let message = LocalTerm::Tuple(vec![self_pid, message]);
            context.messenger.as_mut().unwrap().route_message(*sender, message, 16);

            reductions -= 1;
            if reductions <= 0 {
                return;
            }
        }
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

