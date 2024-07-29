%%% Abstraction for talking to ports. The corresponding Rust abstraction is
%%% implemented in `src/vm/port.rs`.

-module(gen_port).
-export([call/2, call/3, call/4]).

%% Performs a synchronous call to a port with a timeout of 5 seconds and with no
%% access token. For a more detailed description, see `call/4`.
-spec call(Port :: port(), Request :: term()) -> {error, timeout} | Response :: term().
call(Port, Request) -> call(Port, Request, notoken, 5000).

%% Performs a synchronous call to a port with a timeout of 5 seconds. For a more
%% detailed description, see `call/4`.
-spec call(Port :: port(), Request :: term(), Token :: reference()) -> {error, timeout} | Response :: term().
call(Port, Request, Token) -> call(Port, Request, Token, 5000).

%% Performs a synchronous call to a port.
%% 
%% Port calls usually require an access token that must be obtained somehow.
%% Usually, it will be passed down from a parent in the supervision tree. If an
%% access token is not required, `notoken` may be passed to `Token`.
%% 
%% If the port does not respond in `Timeout` milliseconds, `{error, timeout}` is
%% returned.
-spec call(Port :: port(), Request :: term(), Token :: reference() | notoken, Timeout :: integer()) ->
    {error, timeout} | Response :: term().
call(Port, Request, Token, Timeout)
when is_port(Port)
 and (is_reference(Token) or (Token == notoken))
 and is_integer(Timeout) ->
    ConversationId = erlang:make_ref(),
    Port ! {'$gen_port_call', ConversationId, Token, Request},
    receive
        {Port, {ConversationId, Response}} -> Response
    after
        Timeout -> {error, timeout}
    end.
