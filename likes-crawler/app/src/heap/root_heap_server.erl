-module(root_heap_server).

-behaviour(gen_heap).

%%% api
-export([start_link/0, pull/1, push/1]).

%%% behaviour
-export([init/1, pull_up/2, push_up/2]).

-define(SERVER_NAME, {global, ?MODULE}).

-record(state, {last_user}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_heap:start_link(?SERVER_NAME, ?MODULE, []).

pull(Count) -> gen_heap:pull(?SERVER_NAME, Count).

push(Items) -> gen_heap:push(?SERVER_NAME, Items).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  MinSize = 10000,
  NormalSize = 50000,
  MaxSize = infinity,
  NewState = #state{last_user = 0},
  {ok, {MinSize, NormalSize, MaxSize}, NewState}.

pull_up(Count, #state{last_user = LastUser} = State) ->
  lager:debug("pull_up ~B", [Count]),
  {NewLastUser, Jobs} = generate_jobs(LastUser, Count),
  NewState = State#state{last_user = NewLastUser},
  {ok, Jobs, NewState}.

push_up(_Items, State) ->
  lager:warning("Unexpected push_up in heap"),
  {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

generate_jobs(LastUser, Count) -> generate_jobs(LastUser, Count, []).

generate_jobs(LastUser, 0, Acc) ->
  ResultJobs = lists:reverse(Acc),
  {LastUser, ResultJobs};

generate_jobs(LastUser, Count, Acc) ->
  NewLastUser = LastUser + 1000,
  Job = {filter_users, [lists:seq(LastUser + 1, NewLastUser)]},
  generate_jobs(NewLastUser, Count - 1, [Job | Acc]).
