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

    % say hello world
    {error, access} = gen_port:call(LogPort, {write, <<"Hello, World!">>}),
    % uh-oh. we can't! we need an access token

    % acquire the access token (this can only be done once)
    % capability-based security from the ground up!
    {ok, AccessToken} = gen_port:call(LogPort, mint_token),

    % say hello world
    ok = gen_port:call(LogPort, {write, <<"Hello, World!">>}, AccessToken),

    % we can't get a new access token, we have to use the one we acquired
    % earlier.
    {error, access} = gen_port:call(LogPort, mint_token).
