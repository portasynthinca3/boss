-module(gen_perm).
-export_type([token/0, perm_spec/0, state/0]).
-export([permits/2, permits/3, subset/2]).

-moduledoc "
Generic permission / access token system for processes
".

-doc "Access token".
-type token() :: reference() | notoken.
-define(IS_TOKEN(Val), (is_reference(Val) or Val =:= notoken)).
-define(RANGE(Low, High), {'$range', Low, High}).
-define(VALID_RANGE(Low, High), (is_integer(Low) andalso is_integer(High) andalso Low =< High)).

-doc "Access token permission specification".
-type perm_spec() ::
    (Enum :: list(perm_spec())) |
    (IntRange :: ?RANGE(integer(), integer())) |
    (Tuple :: {perm_spec()}) |
    integer() |
    atom().

-doc "Permission map state".
-opaque state() :: #{token() => perm_spec()}.

-doc "Determines whether a token permits a request".
-spec permits(term(), token(), state()) -> boolean().
permits(Request, Token, State) when ?IS_TOKEN(Token) and is_map(State) ->
    maybe
        {ok, Spec} ?= maps:find(Token, State),
        true ?= permits(Request, Spec)
    else
        _ -> false
    end.

-doc "Determines whether a specification permits a request".
-spec permits(term(), perm_spec()) -> boolean().
permits(Request, Spec) when is_list(Spec) -> lists:any(fun(X) -> permits(Request, X) end, Spec);
permits(Request, Spec) when is_integer(Request), is_integer(Spec) -> Request =:= Spec;
permits(Request, ?RANGE(Low, High)) when is_integer(Request), ?VALID_RANGE(Low, High) -> (Request >= Low) and (Request =< High);
permits(Request, Spec) when is_atom(Request), is_atom(Spec) -> Request =:= Spec;
permits(Request, Spec) when is_tuple(Request), is_tuple(Spec), tuple_size(Request) =:= tuple_size(Spec) ->
    lists:all(fun({R, S}) -> permits(R, S) end, lists:zip(tuple_to_list(Request), tuple_to_list(Spec)));
permits(_, _) -> false.

-doc "
Sets the Nth item in a list to a value
".
set_nth(1, [_|Rest], New) -> [New|Rest];
set_nth(I, [E|Rest], New) -> [E|set_nth(I - 1, Rest, New)].

-doc "
Transforms an list of tuples into a tuple of lists of their constituent parts.
In other words, this is a generic unzip operation.
".
unzip_generic(Arity, Input) -> unzip_generic(Arity, Input, lists:duplicate(Arity, [])).
unzip_generic(Arity, [InputItem|InputRest], Output) when tuple_size(InputItem) =:= Arity ->
    NewOut = lists:foldl(fun(I, Acc) ->
        TupleItem = element(I, InputItem),
        ListItem = lists:nth(I, Acc) ++ [TupleItem],
        set_nth(I, Acc, ListItem)
    end, Output, lists:seq(1, Arity)),
    unzip_generic(Arity, InputRest, NewOut);
unzip_generic(_Arity, [], Output) -> list_to_tuple(Output).

-doc "
Takes an enum (union) representation and merges the ranges and integers into
contiguous ranges where possible.
".
flatten_ranges(Enum) ->
    Flattened = lists:flatten(Enum),
    {RangesAndNumbers, Others} = lists:partition(fun
        (X) when is_integer(X) -> true;
        (?RANGE(L, H)) when ?VALID_RANGE(L, H) -> true;
        (_) -> false
    end, Flattened),
    Ranges = lists:map(fun
        (X) when is_integer(X) -> ?RANGE(X, X);
        (X) -> X
    end, RangesAndNumbers),
    Sorted = lists:sort(fun
        (?RANGE(_LowA, HighA), ?RANGE(LowB, _HighB)) -> HighA =< LowB
    end, Ranges),
    Merged = lists:foldl(fun
        (Item, []) -> [Item];
        (?RANGE(Low, _High)=Item, [?RANGE(_LastLow, LastHigh)|_]=Out) when Low - LastHigh > 1 -> [Item|Out];
        (?RANGE(_Low, High)=_Item, [?RANGE(LastLow, _LastHigh)|Tail]) -> [?RANGE(LastLow, High)|Tail]
    end, [], Sorted),
    SinglesRemoved = lists:map(fun
        (?RANGE(Low, High)) when Low =:= High -> Low;
        (X) -> X
    end, Merged),
    SinglesRemoved ++ Others.

-doc "
Determines whether the permission specification `Sub` is a subset (not
necessarily a strict subset) of `Super`.
".
-spec subset(perm_spec(), perm_spec()) -> boolean().
subset(Sub, Super) when is_atom(Sub), is_list(Super) -> lists:any(fun(X) -> subset(Sub, X) end, Super);
subset(Sub, ?RANGE(Low, High)) when is_atom(Sub), ?VALID_RANGE(Low, High) -> false;
subset(Sub, Super) when is_atom(Sub), is_tuple(Super) -> false;
subset(Sub, Super) when is_atom(Sub), is_integer(Super) -> false;
subset(Sub, Super) when is_atom(Sub), is_atom(Super) -> Sub =:= Super;

subset(Sub, Super) when is_integer(Sub), is_list(Super) -> lists:any(fun(X) -> subset(Sub, X) end, Super);
subset(Sub, ?RANGE(Low, High)) when is_integer(Sub), ?VALID_RANGE(Low, High) -> (Sub >= Low) and (Sub =< High);
subset(Sub, Super) when is_integer(Sub), is_integer(Super) -> Sub =:= Super;
subset(Sub, Super) when is_integer(Sub), is_atom(Super) -> false;
subset(Sub, Super) when is_integer(Sub), is_tuple(Super) -> false;

subset(?RANGE(Low, High)=Sub, Super) when ?VALID_RANGE(Low, High), is_list(Super) ->
    lists:any(fun(X) -> subset(Sub, X) end, flatten_ranges(Super));
subset(?RANGE(SubLow, SubHigh), ?RANGE(SuperLow, SuperHigh)) when ?VALID_RANGE(SubLow, SubHigh), ?VALID_RANGE(SuperLow, SuperHigh) ->
    (SubLow >= SuperLow) and (SubHigh =< SuperHigh);
subset(?RANGE(Low, High), Super) when ?VALID_RANGE(Low, High), is_tuple(Super) -> false;
subset(?RANGE(Low, High), Super) when ?VALID_RANGE(Low, High), is_integer(Super) -> (Low =:= High) and (Low =:= Super);
subset(?RANGE(Low, High), Super) when ?VALID_RANGE(Low, High), is_atom(Super) -> false;

subset(Sub, Super) when is_tuple(Sub), is_list(Super) ->
    % thanks to @shdown for helping me figure this out
    ExpectedArity = tuple_size(Sub),
    {Tuples, Rest} = lists:partition(fun(X) -> is_tuple(X) andalso tuple_size(X) =:= ExpectedArity end, Super),
    UnzippedTuples = unzip_generic(ExpectedArity, Tuples),
    Tests = lists:zip(tuple_to_list(Sub), tuple_to_list(UnzippedTuples)),
    lists:all(fun(X) -> subset(Sub, X) end, Rest) and lists:all(fun({X, A}) -> subset(X, A) end, Tests);
subset(Sub, ?RANGE(Low, High)) when is_tuple(Sub), ?VALID_RANGE(Low, High) -> false;
subset(Sub, Super) when is_tuple(Sub), is_tuple(Super), tuple_size(Sub) =:= tuple_size(Super) ->
    lists:all(fun({A, B}) -> subset(A, B) end, lists:zip(erlang:tuple_to_list(Sub), erlang:tuple_to_list(Super)));
subset(Sub, Super) when is_tuple(Sub), is_tuple(Super) -> false;
subset(Sub, Super) when is_tuple(Sub), is_integer(Super) -> false;
subset(Sub, Super) when is_tuple(Sub), is_atom(Super) -> false;

subset(Sub, Super) when is_list(Sub), (is_list(Super) orelse is_tuple(Super) orelse is_integer(Super) orelse is_atom(Super)) ->
    lists:all(fun(X) -> subset(X, Super) end, Sub);

subset(_, _) -> false.

%%% ==========
%%% Unit tests
%%% ==========

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

permits_test_() ->
    [
        ?_assert(permits(hi, hi)),
        ?_assertNot(permits(hi, hello)),

        ?_assert(permits(hi, [hi, hello])),
        ?_assert(permits(hello, [hi, hello])),
        ?_assertNot(permits(hiiii, [hi, hello])),

        ?_assert(permits(1, 1)),
        ?_assertNot(permits(1, 2)),

        ?_assert(permits(-100, ?RANGE(-100, 100))),
        ?_assert(permits(0, ?RANGE(-100, 100))),
        ?_assert(permits(100, ?RANGE(-100, 100))),
        ?_assertNot(permits(-101, ?RANGE(-100, 100))),
        ?_assertNot(permits(101, ?RANGE(-100, 100))),

        ?_assert(permits({a, 1}, {a, [1, 3]})),
        ?_assertNot(permits({a, 2}, {a, [1, 3]}))
    ].

flatten_test_() ->
    [
        ?_assertEqual([?RANGE(8, 20), 6, ?RANGE(1, 4)], flatten_ranges([?RANGE(1, 3), 4, [6, [8]], ?RANGE(9, 20)]))
    ].

unzip_test_() ->
    [
        ?_assertEqual({[a, b], [1, 2]}, unzip_generic(2, [{a, 1}, {b, 2}])),
        ?_assertEqual({[a, b, c], [1, 2, 3]}, unzip_generic(2, [{a, 1}, {b, 2}, {c, 3}])),
        ?_assertEqual({[a, b, c, d], [1, 2, 3, 4]}, unzip_generic(2, [{a, 1}, {b, 2}, {c, 3}, {d, 4}])),
        ?_assertEqual({[a, b], [1, 4], [2, 5], [3, 6]}, unzip_generic(4, [{a, 1, 2, 3}, {b, 4, 5, 6}]))
    ].

subset_test_() ->
    [
        ?_assert(subset(hi, hi)),
        ?_assertNot(subset(hi, hello)),

        ?_assert(subset(hi, [hi, hello])),
        ?_assert(subset(hello, [hi, hello])),
        ?_assertNot(subset(bye, [hi, hello])),
        ?_assert(subset([hi], hi)),
        ?_assertNot(subset([hi, hello], [hi])),
        ?_assert(subset([hi], [hi, hello])),
        ?_assert(subset([hi, [], [[]], [[[]]], [[[[]]]], [[[[[]]]]], [[[[[[]]]]]]], [[[]], hi, [], hello, [[[]]]])), % i'm sorry

        ?_assert(subset(1, 1)),
        ?_assertNot(subset(2, 1)),
        ?_assert(subset(1, ?RANGE(-100, 100))),
        ?_assert(subset(-100, ?RANGE(-100, 100))),
        ?_assert(subset(100, ?RANGE(-100, 100))),
        ?_assertNot(subset(-101, ?RANGE(-100, 100))),
        ?_assertNot(subset(101, ?RANGE(-100, 100))),
        ?_assert(subset(?RANGE(-50, 50), ?RANGE(-100, 100))),
        ?_assertNot(subset(?RANGE(-100, 101), ?RANGE(-100, 100))),
        ?_assertNot(subset(?RANGE(-101, 100), ?RANGE(-100, 100))),
        ?_assert(subset([1, 2, 7], ?RANGE(0, 10))),
        ?_assertNot(subset([1, 2, 7, 13], ?RANGE(0, 10))),

        ?_assert(subset({a, b}, {a, b})),
        ?_assertNot(subset({a}, {a, b})),
        ?_assertNot(subset({a, b}, {a})),
        ?_assert(subset({a, 1}, {a, [0, 1, 2]})),
        ?_assertNot(subset({a, 10}, {a, [0, 1, 2]})),
        ?_assert(subset({a, 1}, {a, ?RANGE(0, 2)})),
        ?_assertNot(subset({a, 10}, {a, ?RANGE(0, 2)})),
        ?_assert(subset({a, [1, 2]}, [{a, 1}, {a, 2}])),
        ?_assert(subset({a, ?RANGE(1, 2)}, [{a, 1}, {a, 2}])),
        ?_assert(subset([{a, 1}, {a, 2}], {a, [1, 2]})),
        ?_assertNot(subset({a, [1, 2, 3]}, [{a, 1}, {a, 2}])),
        ?_assert(subset({a, [1, 2, 3]}, [{a, 1}, {a, [2, 3]}])),
        ?_assert(subset([{a, 1}, {a, [1, 2, 3]}], [{a, 1}, {a, [2, 3]}])),
        ?_assert(subset([{a, 1}, {a, [2, 3]}], [{a, 1}, {a, [2, 3]}]))
    ].

subset_real_world_test_() ->
    MemoryMasterPerms = {[read, write],
        [?RANGE(16#0000_0000_0000_0000, 16#0000_7000_0000_0000),
         ?RANGE(16#ffff_8000_0000_0000, 16#ffff_ffff_ffff_ffff)]},
    LAPIC_Perms = {[read, write], ?RANGE(16#fee0_0000, 16#fee0_0e3f)},
    LAPIC_EOI_Perms = {write, 16#fee0_00b0},
    LAPIC_TPR_Perms = {[read, write], 16#fee0_0080},

    [
        ?_assert(subset(LAPIC_Perms, MemoryMasterPerms)),
        ?_assert(subset(LAPIC_EOI_Perms, LAPIC_Perms)),
        ?_assert(subset(LAPIC_TPR_Perms, LAPIC_Perms)),

        ?_assertNot(subset(LAPIC_TPR_Perms, LAPIC_EOI_Perms)),
        ?_assertNot(subset(LAPIC_Perms, LAPIC_EOI_Perms)),
        ?_assertNot(subset(MemoryMasterPerms, LAPIC_EOI_Perms)),

        ?_assertNot(subset(?RANGE(16#0000_8000_0000_0000, 16#ffff_7fff_ffff_ffff), MemoryMasterPerms))
    ].

-endif.
