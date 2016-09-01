-module(gun_killed_streams_test).

-export([start/0]).

start() ->
  application:ensure_all_started(gun),
  {ok, ConnectionPid} = gun:open("api.vk.com", 80),
  IncompleteStreams = [
    gun:get(ConnectionPid, "/method/utils.getServerTime") || _ <- lists:seq(1, 100)
  ],
  listen(ConnectionPid, IncompleteStreams).


listen(ConnectionPid, IncompleteStreams) ->
  receive
    {gun_down, ConnectionPid, _Protocol, normal, KilledStreams, _UnprocessedStreams} -> #{
      incomplete_streams => IncompleteStreams,
      killed_streams => KilledStreams
    };
    {gun_data, ConnectionPid, Stream, fin, _Data} ->
      listen(ConnectionPid, IncompleteStreams -- [Stream])
  end.
