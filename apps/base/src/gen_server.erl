-module(gen_server).

-moduledoc "
Abstraction for generic servers, i.e. processes that receive requests, transform
their state and send replies.

This module differs significantly from OTP's `gen_server`. Most notably, it
integrates the permission system from `gen_perm`.
".

-type from() :: {Pid :: pid(), Conversation :: reference()}.

-doc "
This callback is invoked when the server process starts 
".
-callback init(Args :: term()) ->
    {ok, State :: term()} |
    {stop, Reason :: term()} |
    ignore.

-callback handle_call(Request :: term(), Token :: gen_perm:token(), From :: from(), State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.

