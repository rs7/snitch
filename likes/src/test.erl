-module(test).

%%% api
-export([loop/0]).

%%%===================================================================
%%% api
%%%===================================================================

loop() ->
  Requests = [{'utils.getServerTime', #{}} || _ <- lists:seq(1, 100)],
  loop(Requests).

loop(Requests) ->

  {Time, Value} = timer:tc(fun vk:run/1, [Requests]),
  io:format("~.3f req/sec~n", [100000000 / Time]),

  loop(Requests).
