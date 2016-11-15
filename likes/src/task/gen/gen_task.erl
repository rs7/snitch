-module(gen_task).

%%% api
-export([behaviour_info/1]).

%%%===================================================================
%%% api
%%%===================================================================

behaviour_info(callbacks) -> [{type, 0}];

behaviour_info(_Other) -> undefined.
