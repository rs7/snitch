-module(list).

%%% api
-export([params/2, params/3, optimize_map/3]).

%%%===================================================================
%%% api
%%%===================================================================

params(Count, Limit) -> params(0, Count, Limit).
params(Offset, Count, Limit) when Count > Limit -> [{Offset, Limit} | params(Offset + Limit, Count - Limit, Limit)];
params(Offset, Count, _Limit) -> [{Offset, Count}].

optimize_map(Offset, Count, Default) -> maps:merge(offset_map(Offset), count_map(Count, Default)).

%%%===================================================================
%%% internal
%%%===================================================================

offset_map(0) -> #{};
offset_map(Offset) -> #{offset => Offset}.

count_map(Count, Default) when Count > Default -> #{count => Count};
count_map(_Count, _Default) -> #{}.
