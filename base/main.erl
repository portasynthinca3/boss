%%% This is the BEAM entry point. So far, BOSS (its Rust part) has:
%%%   - set up a JIT-ed BEAM-like VM;
%%%   - loaded base OS modules (incl. this one);
%%%   - spawned several ports for low-level access to the outside world;
%%%   - spawned a process running main:main/1.
%%% We truly are in a barebones environment, huh. We don't even have access to
%%% the filesystem - it must be implemented by us. In Erlang.
%%% Sometimes i really do have the dumbest ideas come to me.

-module(main).
-vsn(1).
-export([main/1]).

main(_Ports) ->
    ok.
