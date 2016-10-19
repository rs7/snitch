-module(task_manager).

%%% api
-export([reserve/2, release/2]).

%%%===================================================================
%%% api
%%%===================================================================

create(JobData) -> ok.

reserve(request, Count) -> [
  begin
    JobRef = make_ref(),
    JobData = {get_friends, 1},
    {JobRef, JobData}
  end || _ <- lists:seq(1, Count)
].

release(JobRef, Result) -> lager:debug("release job ~p with result ~p", [JobRef, Result]).

retrieve(JobRef) -> lager:debug("retrieve job ~p", [JobRef]).
