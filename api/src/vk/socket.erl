-module(socket).

-export([process/1]).

-define(SNDBUF, 16384).
-define(RECBUF, 65536).
-define(BUFFER, ?SNDBUF + ?RECBUF).

-define(SEND_TIMEOUT, 1000).
-define(CONNECT_TIMEOUT, 1000).
-define(RECEIVE_TIMEOUT, 1000).

-define(OPTIONS, [
  inet,
  binary,
  {active, false},
  {send_timeout, ?SEND_TIMEOUT},
  {sndbuf, ?SNDBUF},
  {recbuf, ?RECBUF},
  {buffer, ?BUFFER}
]).

process(<<>>) -> <<>>;

process(Send) -> connect(Send).

connect(Send) ->
  case ssl:connect('api.vk.com', 443, ?OPTIONS, ?CONNECT_TIMEOUT) of

    {ok, Socket} -> send(Socket, Send);

    {error, _Reason} -> <<>>

  end.

send(Socket, Send) ->
  case ssl:send(Socket, Send) of

    ok -> recv(Socket);

    {error, _Reason} ->
      close(Socket),
      <<>>

  end.

recv(Socket) -> recv(Socket, <<>>).

recv(Socket, Recv) ->
  case ssl:recv(Socket, 0, ?RECEIVE_TIMEOUT) of

    {ok, Packet} -> recv(Socket, <<Recv/binary, Packet/binary>>);

    {error, _Reason} ->
      close(Socket),
      Recv

  end.

close(Socket) -> ssl:close(Socket).
