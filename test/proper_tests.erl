-module(proper_tests).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

property_test_() ->
    Props = lists:filter(fun ({F, _}) -> lists:prefix("prop_", atom_to_list(F)) end,
                         ?MODULE:module_info(exports)),
    Opts = [verbose, {numtests, 10000}],
    Tests = lists:map(fun ({P, _}) ->
                              {atom_to_list(P),
                               {timeout, 30,
                                ?_assert(proper:quickcheck(?MODULE:P(), Opts))}}
                      end, Props),
    
    {foreach,
     fun setup/0, fun teardown/1,
     Tests}.

setup() -> ok.
teardown(_) -> ok.



proper_treap() ->
    ?LET(L, orddict(), treap:from_list(L)).

orddict() ->
    ?LET(L, list({key(), value()}), orddict:from_list(L)).

key() -> atom().
value() -> integer().

with_existing_key() ->
    ?LET({K, V, T}, {key(), value(), proper_treap()},
         {K, treap:store(K, V, T)}).


prop_find() ->
    ?FORALL({K, T}, {key(), proper_treap()},
            begin
                case treap:find(K, T) of
                    {ok, V} ->
                        {K, V} =:= proplists:lookup(K, treap:to_list(T));
                    error ->
                        not proplists:is_defined(K, treap:to_list(T))
                end
            end).


prop_store() ->
    ?FORALL({K, V, T}, {key(), value(), proper_treap()},
            begin
                T2 = treap:store(K, V, T),
                is_heap(T2) andalso {K, V} =:= proplists:lookup(K, treap:to_list(T2))
            end).


prop_erase() ->
    ?FORALL({K, T}, frequency([{1, {key(), proper_treap()}},
                               {2, with_existing_key()}]),
            begin 
                T2 = treap:erase(K, T),
                is_heap(T2) andalso none =:= proplists:lookup(K, treap:to_list(T2))
            end).


prop_is_key() ->
    ?FORALL({K, T}, frequency([{1, {key(), proper_treap()}},
                               {2, with_existing_key()}]),
            treap:is_key(K, T) =:= proplists:is_defined(K, treap:to_list(T))).


prop_split_merge() ->
    ?FORALL({K, T}, with_existing_key(),
                 begin
                    {Left, Right} = treap:split(K, T),
                    is_heap(Left) andalso is_heap(Right) andalso
                        treap:erase(K, T) =:= treap:merge(Left, Right)
                 end).


prop_size() ->
    ?FORALL(T, proper_treap(),
          treap:size(T) =:= length(treap:to_list(T))).


prop_height() ->
    ?FORALL(T, proper_treap(),
            treap:height(T) =< treap:size(T)).


prop_to_list() ->
    ?FORALL(L, orddict(),
            lists:usort(L) =:= treap:to_list(treap:from_list(L))).

prop_from_list() ->
    ?FORALL(L, orddict(),
            begin
                T = treap:from_list(L),
                is_heap(T) andalso L =:= treap:to_list(T)
            end).



%% Check that the heap property is maintained
is_heap(nil) ->
    true;
is_heap({_, _, _, nil, nil}) ->
    true;
is_heap({P, _, _, {LP, _, _, _, _} = Left, nil}) ->
    P > LP andalso is_heap(Left);
is_heap({P, _, _, nil, {RP, _, _, _, _} = Right}) ->
    P > RP andalso is_heap(Right);
is_heap({P, _, _, {LP, _, _, _, _} = Left, {RP, _, _, _, _} = Right}) ->
    P > LP andalso P > RP andalso is_heap(Left) andalso is_heap(Right).

