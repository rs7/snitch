-module(metrics).

%%% api
-export([start_link/0, notify/0]).

-define(TIME, erlang:monotonic_time(millisecond)).
-define(DURATION, 10000).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  Pid = spawn_link(fun metrics_start/0),
  register(?MODULE, Pid),
  {ok, Pid}.

notify() -> ?MODULE ! notify.

%%%===================================================================
%%% internal
%%%===================================================================

metrics_start() -> metrics().

metrics() ->
  StartTime = ?TIME,
  timer:sleep(?DURATION),
  FinishTime = ?TIME,
  Duration = FinishTime - StartTime,
  Count = count(),
  Value = Count / Duration * 1000,
  lager:info("~.3f req/sec", [Value]),
  metrics().

count() -> count(0).

count(Count) ->
  receive
    notify -> count(Count + 1)
  after 0 -> Count
  end.
