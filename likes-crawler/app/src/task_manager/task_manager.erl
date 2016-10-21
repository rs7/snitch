-module(task_manager).

%%% api
-export([reserve/2, release/2, retrieve/1]).

%%%===================================================================
%%% api
%%%===================================================================

create(JobData) -> ok.

reserve(request, Count) -> [
  begin
    JobRef = make_ref(),
    JobData = {get_friends, rand:uniform(390000000)},
    {JobRef, JobData}
  end || _ <- lists:seq(1, Count)
].

release(JobRef, Result) -> ok. %lager:debug("release job ~p", [JobRef]).

retrieve(JobRef) -> lager:debug("retrieve job ~p", [JobRef]).
