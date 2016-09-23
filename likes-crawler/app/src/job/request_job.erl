-module(request_job).

-behavior(job_type).

%%% behavior
-export([process/2]).

%%%===================================================================
%%% behavior
%%%===================================================================

process(Priority, {RequestType, Context}) ->
  Request = RequestType:request(Context),
  {ok, Result} = call_queue:call(Priority, Request),
  {RequestJobs, Jobs} = RequestType:response(Result, Context),
  Response = lists:append([{request_job, RequestJob} || RequestJob <- RequestJobs], Jobs),
  {ok, Response}.
