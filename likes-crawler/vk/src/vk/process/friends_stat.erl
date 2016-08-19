-module(friends_stat).

-behaviour(gen_server).

%%% api
-export([start_link/0, process/2, put_to_stat/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {start_time, count, max, max_user, avg}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put_to_stat(User, Count) -> gen_server:call(?MODULE, {put, User, Count}, infinity).

process(User, Call) ->
  case Call({'friends.get', #{user_id => User, count => 1, v => '5.53'}}) of
    {response, #{<<"count">> := Count}} -> put_to_stat(User, Count);
    {error, 15} -> ok
  end.

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{
    start_time = erlang:timestamp(),
    count = 0,
    max = -1,
    max_user = -1,
    avg = 0.0
  }}.

handle_call({put, User, FriendsCount}, _From, #state{max = Max, count = Count, avg = Avg} = State) ->
  display_progress(State),

  case FriendsCount > Max of
    true ->
      lager:info("New record! ~B! User ~B", [FriendsCount, User]),
      {reply, ok, State#state{
        max = FriendsCount, max_user = User,
        count = Count + 1, avg = (Avg * Count + FriendsCount) / (Count + 1)
      }};

    false ->
      {reply, ok, State#state{count = Count + 1, avg = (Avg * Count + FriendsCount) / (Count + 1)}}
  end;

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
  display_result(State),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

display_progress(#state{count = Count, avg = Avg}) ->
  case Count rem 1000 == 0 of
    true -> lager:info("~B ~f", [Count, Avg]);
    false -> ok
  end.

display_result(#state{max = Max, count = Count, avg = Avg, max_user = MaxUser}) ->
  lager:info(
    "Stoped.~n" ++
      "Scan Count: ~B~n" ++
      "Max Friends Count: ~B~n" ++
      "User: ~B~n" ++
      "Avg Friends Count: ~p",
    [Count, Max, MaxUser, Avg]
  ).
