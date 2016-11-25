-module(list).

%%% api
-export([partition/2, add_page_params/4]).

%%%===================================================================
%%% api
%%%===================================================================

partition(Count, Limit) -> partition(0, Count, Limit).

add_page_params(Params, Offset, Count, DefaultCount) ->
  add_offset_param(add_count_param(Params, Count, DefaultCount), Offset).

%%%===================================================================
%%% internal
%%%===================================================================

partition(Offset, Count, Limit) when Count > Limit ->
  [{Offset, Limit} | partition(Offset + Limit, Count - Limit, Limit)];

partition(Offset, Count, _Limit) -> [{Offset, Count}].

add_count_param(Params, Count, DefaultCount) when Count > DefaultCount -> Params#{count => Count};
add_count_param(Params, _Count, _DefaultCount) -> Params.

add_offset_param(Params, 0) -> Params;
add_offset_param(Params, Offset) -> Params#{offset => Offset}.
