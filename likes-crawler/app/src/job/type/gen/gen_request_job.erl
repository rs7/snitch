-module(gen_request_job).

%%% api
-export([behaviour_info/1]).

%%%===================================================================
%%% api
%%%===================================================================

behaviour_info(callbacks) -> [{request, 1}, {response, 2}];

behaviour_info(_Other) -> undefined.
