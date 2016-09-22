-module(call_queue_data).

%%% api
-export([new/0, add/3, take/2]).

%%%===================================================================
%%% api
%%%===================================================================

new() -> gb_trees:empty().

add(Priority, Value, Data) -> gb_trees:enter(Priority, Value, Data).

take(Count, Data) -> take(Count, Data, []).

%%%===================================================================
%%% internal
%%%===================================================================

take(0, Data, Acc) -> {lists:reverse(Acc), Data};

take(Count, Data, Acc) ->
  case gb_trees:is_empty(Data) of
    true -> take(0, Data, Acc);
    false ->
      {Key, Value, NewData} = gb_trees:take_smallest(Data),
      take(Count - 1, NewData, [{Key, Value} | Acc])
  end.
