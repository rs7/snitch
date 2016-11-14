-module(multi).

-export([speed/1]).

-export([start/1]).

speed(Count) ->
  {Time, Value} = timer:tc(?MODULE, start, [Count]),

  Total = 100000000 * Count / Time,
  io:format("total ~.3f req/sec~n", [Total]),

  ok.

start(Count) -> rpc:parallel_eval([{test, start, []} || _ <- lists:seq(1, Count)]).
