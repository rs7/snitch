-module(gen_query_task).

%%% api
-export([behaviour_info/1]).

%%%===================================================================
%%% api
%%%===================================================================

behaviour_info(callbacks) -> [{query, 1}, {result, 2}];

behaviour_info(_Other) -> undefined.
