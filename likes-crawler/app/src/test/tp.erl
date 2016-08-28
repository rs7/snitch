-module(tp).

-export([start/0]).
-export([test/1]).

open() -> gun:open("api.vk.com", 80, #{
  retry => 1,
  retry_timeout => 0,
  http_opts => #{
    keepalive => 16#7FFFFFF
  }
}).

close(Pid) -> gun:shutdown(Pid).

req(Pid) -> gun:get(Pid, "/method/utils.getServerTime").

test(Fun) ->
  {ok, Pid} = open(),
  {Time, _Value} = timer:tc(Fun, [Pid]),
  close(Pid),
  io:format("~p ", [Time / 1000]),
  Time / 1000.

start() ->
  application:ensure_all_started(gun),
  io:format("loading... ", []),
%%  rpc:parallel_eval([
%%    {?MODULE, test, [fun req100/1]} || _ <- lists:seq(1, 100)
%%  ]).
  [test(fun req100/1) || _ <- lists:seq(1, 10)].

req100(Pid) -> MonitorRef = monitor(process, Pid), [gun:await_body(Pid, req(Pid), MonitorRef) || _ <- lists:seq(1, 99)], ok.
