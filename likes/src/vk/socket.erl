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

process(Send) -> connect(Send).

connect(Send) ->
  case gen_tcp:connect('api.vk.com', 80, ?OPTIONS, ?CONNECT_TIMEOUT) of

    {ok, Socket} -> send(Socket, Send);

    {error, _Reason} ->
      lager:info("Connect error ~p", [_Reason]),
      <<>>

  end.

send(Socket, Send) ->
  case gen_tcp:send(Socket, Send) of

    ok -> recv(Socket);

    {error, _Reason} ->
      lager:info("Send error ~p", [_Reason]),
      close(Socket),
      <<>>

  end.

recv(Socket) -> recv(Socket, <<>>).

recv(Socket, Recv) ->
  case gen_tcp:recv(Socket, 0, ?RECEIVE_TIMEOUT) of

    {ok, Packet} -> recv(Socket, <<Recv/binary, Packet/binary>>);

    {error, closed} ->
      close(Socket),
      Recv;

    {error, _Reason} ->
      lager:info("Receive error ~p", [_Reason]),
      close(Socket),
      Recv

  end.

close(Socket) -> gen_tcp:close(Socket).
