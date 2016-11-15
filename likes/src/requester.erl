-module(requester).

-export([loop/0]).

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
  Result = connect(),
  ok.

connect() ->
  case gen_tcp:connect('api.vk.com', 80, ?OPTIONS, 1000) of

    {ok, Socket} -> send(Socket);

    {error, Reason} -> {error, Reason}

  end.

send(Socket) ->
  Send = data(),

  case gen_tcp:send(Socket, Send) of

    ok -> recv(Socket);

    {error, Reason} ->
      close(Socket),
      {error, Reason}

  end.

recv(Socket) -> recv(Socket, []).

recv(Socket, Recv) ->
  case gen_tcp:recv(Socket, 0, 1000) of

    {ok, Packet} -> recv(Socket, [Packet | Recv]);

    {error, Reason} ->
      close(Socket),
      {ok, Recv}

  end.

close(Socket) -> gen_tcp:close(Socket).
