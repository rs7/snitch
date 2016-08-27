-module(test).

-export([start/0]).

-export([test/0]).

-record(state, {pid, monitor}).

start() ->
  application:ensure_all_started(gun),
  test().

test() ->
  flush(),
  io:format("~p ~n", [{test, node()}]),
  Pid = spawn_link(fun() ->
    loop(#state{})
  end),
  register(test, Pid).

loop(#state{pid = Pid, monitor = MonitorRef} = State) ->

  Message = receive M -> M end,

  %io:format(">> ~p~n", [Message]),

  case Message of
    {gun_up, Pid, _Protocol} -> loop(State);

    {gun_down, Pid, _Protocol, _Reason, _KilledStreams, _UnprocessedStreams} ->
      io:format(">> ~p~n", [Message]),
      loop(State);

    {gun_response, Pid, _StreamRef, _IsFin, _Status, _Headers} -> loop(State);

    {gun_data, Pid, _StreamRef, nofin, _Data} -> loop(State);

    {gun_data, Pid, _StreamRef, fin, _Data} -> loop(State);

    {'DOWN', MonitorRef, process, Pid, _Reason} ->
      erlang:demonitor(MonitorRef),
      ok;

    open ->
      {ok, NewPid} = gun:open("api.vk.com", 80, #{
        retry => 1,
        retry_timeout => 0,
        http_opts => #{keepalive => 16#7FFFFFF}
      }),
      NewMonitorRef = monitor(process, NewPid),
      loop(State#state{pid = NewPid, monitor = NewMonitorRef});

    request ->
      gun:post(
        Pid,
        "/method/friends.get",
        [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
        [<<"user_id=">>, integer_to_binary(rand:uniform(380000000))]
      ),
      loop(State);

    close ->
      gun:shutdown(Pid),
      loop(State);

    _UnexpectedMessage -> loop(State)
  end.

flush() -> receive _ -> flush() after 0 -> ok end.
