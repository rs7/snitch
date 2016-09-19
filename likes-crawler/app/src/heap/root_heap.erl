-module(root_heap).

-behaviour(gen_heap).

%%% api
-export([start_link/0, pull/1, push/1]).

%%% behaviour
-export([init/1, pull_from_up_level/2, push_to_up_level/2]).

-define(FILTER_USERS_LIST_LENGTH, 1000).
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
  MinSize = 5000,
  NormalSize = 20000,
  MaxSize = infinity,
  NewState = #state{last_user = 0},
  {ok, {MinSize, NormalSize, MaxSize}, NewState}.

pull_from_up_level(Count, #state{last_user = LastUser} = State) ->
  {NewLastUser, Jobs} = generate_jobs(LastUser, Count),
  NewState = State#state{last_user = NewLastUser},
  {ok, Jobs, NewState}.

push_to_up_level(_Items, _State) -> erlang:error("Unexpected push_to_up_level in root_heap").

%%%===================================================================
%%% internal
%%%===================================================================

generate_jobs(LastUser, Count) -> generate_jobs(LastUser, Count, []).

generate_jobs(LastUser, 0, Acc) ->
  ResultJobs = lists:reverse(Acc),
  {LastUser, ResultJobs};

generate_jobs(LastUser, Count, Acc) ->
  NewLastUser = LastUser + ?FILTER_USERS_LIST_LENGTH,
  Job = {filter_users, [lists:seq(LastUser + 1, NewLastUser)]},
  generate_jobs(NewLastUser, Count - 1, [Job | Acc]).
