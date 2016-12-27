-module(gen_request_task).

%%% api
-export([behaviour_info/1, process/2]).

%%%===================================================================
%%% api
%%%===================================================================

behaviour_info(callbacks) -> [{request, 1}, {response, 2}];

behaviour_info(_Other) -> undefined.

process(Type, Args) ->
  Request = Type:request(Args),
  Response = api_rpc:call(Request),
  Result = Type:response(Response, Args),
  Result.