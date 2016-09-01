-module(gun_error_test).

-export([start/0]).

start() ->
  application:ensure_all_started(gun),

  {ok, ConnectionPid} = connection_lib:open(),
  ConnectionMonitorRef = monitor(process, ConnectionPid),
  listen(ConnectionPid, ConnectionMonitorRef).

listen(ConnectionPid, ConnectionMonitorRef) ->
  receive
    {'DOWN', ConnectionMonitorRef, process, ConnectionPid, Reason} ->
      log("'DOWN' ~p", [Reason]),
      start(),
      ok;

    {gun_error, ConnectionPid, _StreamRef, Reason} ->
      log("gun_stream_error ~p", [Reason]),
      ok;

    {gun_error, ConnectionPid, Reason} ->
      log("gun_error ~p", [Reason]),
      ok;

    {gun_data, ConnectionPid, _StreamRef, fin, _Data} ->
      io:format("."),
      request(ConnectionPid),
      ok;

    {gun_up, ConnectionPid, http} ->
      [request(ConnectionPid) || _ <- lists:seq(1, 5)],
      ok;

    {gun_down, ConnectionPid, _Protocol, Reason, _KilledStreams, _UnprocessedStreams} ->
      log("gun_down ~p", [Reason]),
      ok
  end,
  listen(ConnectionPid, ConnectionMonitorRef).

request(ConnectionPid) -> connection_lib:request(ConnectionPid, {'/method/utils.getServerTime', #{}}).

log(Template, Parameters) -> io:format("|" ++ Template ++ "|", Parameters).
