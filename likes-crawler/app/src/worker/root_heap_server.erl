-module(root_heap_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, generate_jobs/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(FILTER_USERS_SIZE, 1000).
-define(SERVER_NAME, {global, ?MODULE}).

-record(state, {last_user}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?SERVER_NAME, ?MODULE, [], []).

generate_jobs(Count) -> gen_server:call(?SERVER_NAME, {generate_jobs, Count}, infinity).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  NewState = #state{last_user = 0},
  {ok, NewState}.

handle_call({generate_jobs, Count}, _From, #state{last_user = LastUser} = State
) ->
  lager:debug("generate_jobs ~B", [Count]),

  {NewLastUser, Jobs} = generate_jobs(LastUser, Count),
  NewState = State#state{last_user = NewLastUser},
  {reply, {ok, Jobs}, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

generate_jobs(LastUser, Count) -> generate_jobs(LastUser, Count, []).

generate_jobs(LastUser, 0, Acc) -> {LastUser, Acc};

generate_jobs(LastUser, Count, Acc) ->
  NewLastUser = LastUser + ?FILTER_USERS_SIZE,
  Job = {filter_users, [lists:seq(LastUser + 1, NewLastUser)]},
  generate_jobs(NewLastUser, Count - 1, [Job | Acc]).
