-module(metrics).

%%% api
-export([start_link/0, inc/1, inc/2]).

-define(TIME, erlang:monotonic_time(second)).
-define(DURATION, 10000).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  Pid = spawn_link(fun start/0),
  register(?MODULE, Pid),
  {ok, Pid}.

inc(Name) -> inc(Name, 1).

inc(Name, Increment) -> ?MODULE ! {inc, Name, Increment}.

%%%===================================================================
%%% internal
%%%===================================================================

start() ->
  {ok, ConnectionCount} = application:get_env(requester_count),
  loop(ConnectionCount).

loop(ConnectionCount) ->
  StartTime = ?TIME,
  receive after ?DURATION -> ok end,
  FinishTime = ?TIME,
  Values = values(),
  Duration = FinishTime - StartTime,
  process(Values, Duration, ConnectionCount),
  loop(ConnectionCount).

values() ->
  values(dict:new()).

values(Values) ->
  receive
    {inc, Name, Increment} -> values(dict:update_counter(Name, Increment, Values))
  after 0 -> dict:to_list(Values)
  end.

process(Values, Duration, ConnectionCount) ->
  {Format, Args} = lists:unzip([
    process(Name, Value, Duration, ConnectionCount) || {Name, Value} <- Values
  ]),

  lager:info(
    "--------------~n" ++ string:join(Format, "~n"),
    lists:append(Args)
  ),

  ok.

process(Name, Value, Duration, ConnectionCount) -> {
  "~.3f ~p/sec/con ~.3f ~p/sec",
  [
    Value / Duration / ConnectionCount, Name,
    Value / Duration, Name
  ]
}.
