-module(request_job).

%%% api
-export([process/1]).

%%%===================================================================
%%% api
%%%===================================================================

process({Type, Context}) ->
  Priority = 1,
  {ok, Result} = call_queue:call(Priority, Type:request(Context)),
  {ok, [{?MODULE, RequestJob} || RequestJob <- Type:response(Result, Context)]}.
