-module(treap).

-define(RAND_MAX, 1.0).

-export([new/0,
         find/2,
         fetch/2,
         erase/2,
         store/3,
         store/4,
         is_key/2,
         merge/2,
         split/2,
         size/1,
         height/1,
         to_list/1,
         from_list/1]).

-type treap() :: nil | {P :: integer(),
                        Key :: term(),
                        Value :: term(),
                        Left :: treap(),
                        Right :: treap()}.
-export_type([treap/0]).


-spec new() -> treap().
new() ->
    nil.


-spec find(term(), treap()) -> {ok, term()} | error.
find(Key, Treap) ->
    case find_node(Key, Treap) of
        {ok, {_, _, Value, _, _}} -> {ok, Value};
        error                     -> error
    end.


-spec fetch(term(), treap()) -> term() | no_return().
fetch(Key, Treap) ->
    case find(Key, Treap) of
        {ok, Value} -> Value;
        error       -> erlang:error(badarg)
    end.


-spec erase(term(), treap()) -> treap().
erase(Key, {P, K, V, Left, Right}) when Key < K ->
    {P, K, V, erase(Key, Left), Right};

erase(Key, {P, K, V, Left, Right}) when Key > K ->
    {P, K, V, Left, erase(Key, Right)};

erase(_, Treap) ->
    erase_root(Treap).


-spec store(term(), term(), treap()) -> treap().
store(Key, Value, Treap) ->
    store_1(Key, Value, random:uniform(), Treap).


-spec store(term(), term(), float(), treap()) -> treap().
store(Key, Value, Prio, Treap) when Prio >= 0.0 andalso Prio =< 1.0 ->
    store_1(Key, Value, Prio, erase(Key, Treap)).


-spec is_key(term(), treap()) -> boolean().
is_key(Key, Treap) ->
    case find_node(Key, Treap) of
        {ok, _Node} -> true;
        error       -> false
    end.


-spec merge(treap(), treap()) -> treap().
merge(Left, Right) ->
    %% NOTE: assumes max_key(Left) < min_key(Right), i.e the result of split/2
    erase_root({?RAND_MAX + 1, root, undefined, Left, Right}).
    

-spec split(term(), treap()) -> {treap(), treap()}.
split(Key, Treap) ->
    {_, Key, undefined, Left, Right} = store_1(Key, undefined, ?RAND_MAX + 1,
                                               erase(Key, Treap)),
    {Left, Right}.


-spec size(treap()) -> non_neg_integer().
size(nil) -> 0;
size({_, _, _, nil, nil}) -> 1;
size({_, _, _, Left, Right}) -> 1 + ?MODULE:size(Left) + ?MODULE:size(Right).


-spec height(treap()) -> non_neg_integer().
height(nil) -> 0;
height({_, _, _, Left, Right}) -> 1 + max(height(Left), height(Right)).


-spec to_list(treap()) -> [{term(), term()}].
to_list(Treap) ->
    to_list(Treap, []).

to_list({_, Key, Value, Left, Right}, List) ->
    to_list(Left, [{Key, Value} | to_list(Right, List)]);

to_list(nil, List) ->
    List.


-spec from_list(list()) -> treap().
from_list(List) ->
    lists:foldl(fun ({Key, Value}, T) ->
                        store(Key, Value, T);
                    (Key, T) ->
                        store(Key, true, T)
                end, new(), List).


%%
%% INTERNAL
%%


erase_root(nil) -> nil;

erase_root({_, _ ,_, Left, nil}) -> Left;

erase_root({_, _ ,_, nil, Right}) -> Right;

erase_root({_, _, _, {LeftP, _, _, _, _}, {RightP, _, _, _, _}} = T) ->
    case LeftP > RightP of
        true ->
            {P, K, V, Left, Right} = rot_r(T),
            {P, K, V, Left, erase_root(Right)};
        false ->
            {P, K, V, Left, Right} = rot_l(T),
            {P, K, V, erase_root(Left), Right}
    end.


store_1(Key, Value, Prio, {P, K, V, Left, Right}) when Key < K ->
    maybe_rot_r({P, K, V, store_1(Key, Value, Prio, Left), Right});

store_1(Key, Value, Prio, {P, K, V, Left, Right}) when Key > K ->
    maybe_rot_l({P, K, V, Left, store_1(Key, Value, Prio, Right)});

store_1(_, Value, _, {P, Key, _, Left, Right}) ->
    {P, Key, Value, Left, Right}; %% update

store_1(Key, Value, Prio, nil) ->
    {Prio, Key, Value, nil, nil}. %% insert


maybe_rot_r({P, _, _, {LeftP, _, _, _, _}, _} = T) when LeftP > P -> rot_r(T);
maybe_rot_r(T) -> T.

rot_r({P, K, V, {LeftP, LeftK, LeftV, L, R}, Right}) ->
    {LeftP, LeftK, LeftV, L, {P, K, V, R, Right}}.


maybe_rot_l({P, _, _, _, {RightP, _, _, _, _}} = T) when RightP > P -> rot_l(T);
maybe_rot_l(T) -> T.

rot_l({P, K, V, Left, {RightP, RightK, RightV, L, R}}) ->
    {RightP, RightK, RightV, {P, K, V, Left, L}, R}.


find_node(Key, {_, OtherKey, _, Left, _}) when Key < OtherKey ->
    find_node(Key, Left);

find_node(Key, {_, OtherKey, _, _, Right}) when Key > OtherKey ->
    find_node(Key, Right);

find_node(_, {_, _, _, _, _} = Node) ->
    {ok, Node};

find_node(_, nil) ->
    error.
