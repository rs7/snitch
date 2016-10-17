-module(gen_query_job).

%%% api
-export([behaviour_info/1]).

%%%===================================================================
%%% api
%%%===================================================================

behaviour_info(callbacks) -> [{query, 1}];

behaviour_info(_Other) -> undefined.
