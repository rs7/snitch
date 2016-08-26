-module(test).

%%% api
-export([start/3]).

-export([test/1]).

%%%===================================================================
%%% api
%%%===================================================================

start(From, To, Inc) ->
  application:ensure_all_started(gun),
  rpc:parallel_eval([{?MODULE, test, [Delay]} || Delay <- lists:seq(From, To, Inc)]).

search(X, Y) when Y - X =< 1000 -> {X, Y};
search(From, To) ->
  Current = (From + To) div 2,
  case test(Current) of
    ok -> search(Current, To);
    down -> search(From, Current)
  end.

test(Delay) ->

  flush(),

  io:format(">>> test ~b ~n", [Delay]),

  {ok, Pid} = gun:open("api.vk.com", 80, #{
    retry => 0,
    retry_timeout => 1000000,
    trace => false,
    http_opts => #{
      keepalive => 99999
    }
  }),

  MonitorRef = monitor(process, Pid),

  self() ! send_request,

  N = loop(#{
    pid => Pid,
    monitor => MonitorRef,
    n => 0,
    delay => Delay
  }),

  io:format(">>> result ~b --> ~p~n", [Delay, N]),

  {N, Delay}.

current_time() -> erlang:monotonic_time(seconds).

loop(#{pid := Pid, monitor := MonitorRef, n := N, delay := Delay} = State) ->

  Message = receive M -> M end,

  case Message of
    {gun_up, Pid, _Protocol} ->
      loop(State#{
        start_time => current_time()
      });

    {gun_down, Pid, _Protocol, _Reason, _KilledStreams, _UnprocessedStreams} -> loop(State);
    {gun_response, Pid, _StreamRef, _IsFin, _Status, _Headers} -> loop(State);
    {gun_data, Pid, _StreamRef, nofin, _Data} -> loop(State);

    {gun_data, Pid, _StreamRef, fin, _Data} ->
      erlang:send_after(Delay, self(), send_request),
      loop(State#{n => N + 1});

    {'DOWN', MonitorRef, process, Pid, _Reason} -> N;

    send_request ->
      send_request(Pid),
      loop(State);

    UnexpectedMessage ->
      log({unexpected_message, UnexpectedMessage}),
      loop(State)
  end.

flush() -> receive _ -> flush() after 0 -> ok end.

send_request(Pid) ->
  gun:post(
    Pid,
    "/method/utils.getServerTime",
    [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    <<>>
  ).

log(Msg) -> io:format(">> ~p~n", [Msg]).
