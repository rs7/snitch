-module(test).

-export([loop/0, tick/0]).

-define(REQUEST, <<"GET /method/utils.getServerTime HTTP/1.1\nHost: api.vk.com\n\n">>).

-define(SNDBUF, 16384).
-define(RECBUF, 65536).
-define(BUFFER, ?SNDBUF + ?RECBUF).

-define(OPTIONS, [
  inet,
  binary,
  {active, false},
  {sndbuf, ?SNDBUF},
  {recbuf, ?RECBUF},
  {buffer, ?BUFFER}
]).

loop() ->

  {RawTime, Value} = timer:tc(fun tick/0),

  Time = RawTime / 1000000,
  Speed = 100 / Time,
  io:format("~.3f sec ~.3f req/sec~n", [Time, Speed]),

  loop().

tick() ->
  Result = connect(),
  io:format("~n", []),
  ok.

connect() ->
  case gen_tcp:connect('api.vk.com', 80, ?OPTIONS, 1000) of

    {ok, Socket} ->
      io:format("@", []),
      send(Socket);

    {error, Reason} ->
      io:format("/~p/", [Reason]),
      {error, Reason}

  end.

send(Socket) ->
  Send = data(),

  case gen_tcp:send(Socket, Send) of

    ok ->
      io:format("o", []),
      recv(Socket, []);

    {error, Reason} ->
      io:format("/~p/", [Reason]),
      close(Socket),
      {error, Reason}

  end.

recv(Socket, Recv) ->
  case gen_tcp:recv(Socket, 0, 1000) of

    {ok, Packet} ->
      io:format("-", []),
      recv(Socket, [Packet | Recv]);

    {error, closed} ->
      io:format("x", []),
      close(Socket),
      {ok, Recv};

    {error, Reason} ->
      io:format("/~p/", [Reason]),
      close(Socket),
      {ok, Recv}

  end.

close(Socket) ->
  gen_tcp:close(Socket),
  io:format("#", []).

data() -> data([], 100).

data(Acc, 0) -> Acc;

data(Acc, Count) -> data([?REQUEST | Acc], Count - 1).
