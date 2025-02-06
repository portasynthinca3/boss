-module(tests_runner).
-export([run_tests/0]).

run_tests() ->
    gen_perm:test().
