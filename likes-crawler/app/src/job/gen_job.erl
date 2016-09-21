-module(gen_job).

%%% api
-export([behaviour_info/1]).

%%%===================================================================
%%% api
%%%===================================================================

behaviour_info(callbacks) -> [{process, 1}];

behaviour_info(_Other) -> undefined.
