%%% This is the BEAM entry point. So far, BOSS (its Rust part) has:
%%%   - set up a BEAM-like VM;
%%%   - loaded base OS modules (incl. this one);
%%%   - spawned several ports for low-level access to the outside world;
%%%   - spawned a process running 'base:main':main/2.
%%% We truly are in a barebones environment, huh. We don't even have access to
%%% the filesystem - it must be implemented by us. In Erlang.
%%% Sometimes i really do have the dumbest ideas come to me.

-module(main).
-export([main/2]).

main(Config, Ports) ->
    % assert platform, get log port
    #{platform := 'x86_64-uefi'} = Config,
    #{log_port := LogPort} = Ports,

    % acquire access token (this can only be done once)
    % capability-based security from the ground up!
    ConversationId = erlang:make_ref(),
    LogPort ! {ConversationId, mint_token, [], []},
    % notice how we don't sent our pid explicitly: the emulator does that for us
    AccessToken = receive
        {LogPort, {ConversationId, {ok, Token}}} -> Token
    end,

    % finally write hello world
    % a new conversation id may be generated, but that's not required here
    LogPort ! {ConversationId, write, AccessToken, [<<"Hello, World!">>]},
    receive
        {LogPort, {ConversationId, ok}} -> ok;
        {LogPort, {ConversationId, {error, Err}}} ->
            % welp!
            % i dunno, let's raise a badmatch exception to crash the emulator
            ok = {error, Err}
    end.
