-module(gen_request_job).

%%% api
-export([behaviour_info/1]).

%%%===================================================================
%%% api
%%%===================================================================

behaviour_info(callbacks) -> [{request, 1}, {response, 2}];

behaviour_info(_Other) -> undefined.

%%process(Module, Priority, Context) ->
%%  Request = Module:request(Context),
%%  {ok, Result} = call_queue:call(Priority, Request),
%%  Response = Module:response(Result, Context),
%%  {ok, Response}.
