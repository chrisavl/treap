-module(proper_statem_tests).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


proper_test_() ->
    {timeout, 60, ?_assert(proper:quickcheck(?MODULE:prop_treap(), 10000))}.


prop_treap() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
               begin
                   {History,State,Result} = run_commands(?MODULE, Cmds),

                   ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                       [History, State, Result]),
                             aggregate(command_names(Cmds), Result =:= ok))
                end)).

treap({T, _}) -> T.

contents({_, C}) -> C.

key_value_pair(S) ->
    case contents(S) =:= [] of
        true  -> {key(), value()};
        false -> frequency([{1, {key(), value()}},
                            {2, elements(contents(S))}])
    end.

key() -> atom().

value() -> integer().


command(S) ->
    ?LET({T, {K, V}}, {treap(S), key_value_pair(S)},
         oneof([{call, treap, find, [K, T]},
                {call, treap, erase, [K, T]},
                {call, treap, store, [K, V, T]},
                {call, treap, store, [K, V, float(), T]},
                {call, treap, is_key, [K, T]}
               ])).

initial_state() ->
    {treap:new(), []}.


precondition(_S, _) ->
    true.


next_state(S, T, {call, _, store, [K, V | _]}) ->
    {T, lists:keystore(K, 1, contents(S), {K, V})}; 

next_state(S, T, {call, _, erase, [K, _]}) ->
    {T, lists:keydelete(K, 1, contents(S))};

next_state(S, _, _) ->
    S.


postcondition(S, {call, _, find, [K, _]}, error) ->
    not lists:keymember(K, 1, contents(S));

postcondition(S, {call, _, find, [K, _]}, {ok, V}) ->
    {K, V} =:= proplists:lookup(K, contents(S));

postcondition(_, {call, _, erase, [_, _]}, T) ->
    proper_tests:is_heap(T);

postcondition(_, {call, _, store, [_, _ | _]}, T) ->
    proper_tests:is_heap(T);

postcondition(S, {call, _, is_key, [K, _]}, B) ->
    B =:= proplists:is_defined(K, contents(S)).
