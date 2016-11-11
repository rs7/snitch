-module(test).

-export([start/1]).

-include_lib("test.hrl").

-define(OPTIONS, [
  inet,
  binary,
  {active, false},
  {nodelay, true}
]).

start(N) ->
  Time = series(N, 0),
  Value = (N * 100) / (Time / 1000000),
  ?OUT("~p req/sec~n", [Value]).

series(0, Time) -> Time;

series(N, AccTime) ->
  case timer:tc(fun tick/0) of

    {Time, ok} -> series(N - 1, AccTime + Time);

    {_Time, Result} ->
      ?PRINT([Result]),
      series(N, AccTime)

  end.

tick() -> connect().

connect() ->
  ?LOG,
  case gen_tcp:connect('api.vk.com', 80, ?OPTIONS) of

    {ok, Socket} -> send(Socket);

    {error, Reason} -> {error, Reason}

  end.

send(Socket) ->
  ?LOG,
  Data = [?REQUEST || _ <- lists:seq(1, 100)],

  case gen_tcp:send(Socket, Data) of

    ok -> recv(Socket);

    {error, Reason} ->
      close(Socket),
      {error, Reason}

  end.

recv(Socket) ->
  ?LOG,
  case gen_tcp:recv(Socket, 0) of

    {error, closed} -> ok;

    {error, Reason} ->
      close(Socket),
      {error, Reason};

    {ok, Packet} -> recv(Socket)

  end.

close(Socket) -> gen_tcp:close(Socket).
