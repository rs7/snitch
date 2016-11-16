-module(socket).

-export([process/1]).

-define(SNDBUF, 16384).
-define(RECBUF, 65536).
-define(BUFFER, ?SNDBUF + ?RECBUF).

-define(OPTIONS, [
  inet,
  binary,
  {active, false},
  {send_timeout, 1000},
  {sndbuf, ?SNDBUF},
  {recbuf, ?RECBUF},
  {buffer, ?BUFFER}
]).

process(Send) -> connect(Send).

connect(Send) ->
  case gen_tcp:connect('api.vk.com', 80, ?OPTIONS, 1000) of

    {ok, Socket} -> send(Socket, Send);

    {error, Reason} -> <<>>

  end.

send(Socket, Send) ->
  case gen_tcp:send(Socket, Send) of

    ok -> recv(Socket);

    {error, Reason} ->
      close(Socket),
      <<>>

  end.

recv(Socket) -> recv(Socket, <<>>).

recv(Socket, Recv) ->
  case gen_tcp:recv(Socket, 0, 1000) of

    {ok, Packet} -> recv(Socket, <<Recv/binary, Packet/binary>>);

    {error, _Reason} ->
      close(Socket),
      Recv

  end.

close(Socket) -> gen_tcp:close(Socket).
