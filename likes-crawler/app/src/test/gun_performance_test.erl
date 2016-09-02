-module(gun_performance_test).

-export([start/0]).

-define(COUNT, 1000).

start() ->
  application:ensure_all_started(gun),

  {ok, ConnectionPid} = connection_lib:open(),
  ConnectionMonitorRef = monitor(process, ConnectionPid),
  #{
    test1 => test1(ConnectionPid, ConnectionMonitorRef),
    test100 => test100(ConnectionPid, ConnectionMonitorRef)
  }.

test100(ConnectionPid, ConnectionMonitorRef) ->
  io:format("~n|100|~n"),
  {Time,ok} = timer:tc(fun listen100/3, [ConnectionPid, ConnectionMonitorRef, 0]),
  Time / 1.0e6.

send100(ConnectionPid) -> [send1(ConnectionPid) || _ <- lists:seq(1, 100)].

listen100(_ConnectionPid, _ConnectionMonitorRef, ?COUNT) -> ok;

listen100(ConnectionPid, ConnectionMonitorRef, Count) ->
  receive
    {'DOWN', ConnectionMonitorRef, process, ConnectionPid, Reason} ->
      io:format("~n'DOWN' ~p", [Reason]),
      listen100(ConnectionPid, ConnectionMonitorRef, Count);

    {gun_error, ConnectionPid, _StreamRef, Reason} ->
      io:format("e(~p)", [Reason]),
      listen100(ConnectionPid, ConnectionMonitorRef, Count);

    {gun_error, ConnectionPid, Reason} ->
      io:format("E(~p)", [Reason]),
      listen100(ConnectionPid, ConnectionMonitorRef, Count);

    {gun_data, ConnectionPid, _StreamRef, fin, _Data} ->
      io:format("-"),
      listen100(ConnectionPid, ConnectionMonitorRef, Count);

    {gun_up, ConnectionPid, http} ->
      io:format("/"),
      send100(ConnectionPid),
      listen100(ConnectionPid, ConnectionMonitorRef, Count + 100);

    {gun_down, ConnectionPid, _Protocol, _Reason, _KilledStreams, _UnprocessedStreams} ->
      io:format("\\"),
      listen100(ConnectionPid, ConnectionMonitorRef, Count)
  end.

test1(ConnectionPid, ConnectionMonitorRef) ->
  io:format("~n|1|~n"),
  send1(ConnectionPid),
  {Time,ok} = timer:tc(fun listen1/3, [ConnectionPid, ConnectionMonitorRef, 0]),
  Time / 1.0e6.

send1(ConnectionPid) -> connection_lib:request(ConnectionPid, mock:get_request_data()).

listen1(_ConnectionPid, _ConnectionMonitorRef, ?COUNT) -> ok;

listen1(ConnectionPid, ConnectionMonitorRef, Count) ->
  receive
    {'DOWN', ConnectionMonitorRef, process, ConnectionPid, Reason} ->
      io:format("~n'DOWN' ~p", [Reason]),
      listen1(ConnectionPid, ConnectionMonitorRef, Count);

    {gun_error, ConnectionPid, _StreamRef, Reason} ->
      io:format("e(~p)", [Reason]),
      listen1(ConnectionPid, ConnectionMonitorRef, Count);

    {gun_error, ConnectionPid, Reason} ->
      io:format("E(~p)", [Reason]),
      listen1(ConnectionPid, ConnectionMonitorRef, Count);

    {gun_data, ConnectionPid, _StreamRef, fin, _Data} ->
      io:format("-"),
      send1(ConnectionPid),
      listen1(ConnectionPid, ConnectionMonitorRef, Count + 1);

    {gun_up, ConnectionPid, http} ->
      io:format("/"),
      listen1(ConnectionPid, ConnectionMonitorRef, Count);

    {gun_down, ConnectionPid, _Protocol, _Reason, _KilledStreams, _UnprocessedStreams} ->
      io:format("\\"),
      listen1(ConnectionPid, ConnectionMonitorRef, Count)
  end.
