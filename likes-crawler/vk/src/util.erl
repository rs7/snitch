-module(util).

%% API exports
-export([ceil/1, sum/1, avg/1]).

%%====================================================================
%% API functions
%%====================================================================

ceil(Number) ->
  TruncateNumber = trunc(Number),
  case TruncateNumber == Number of
    true -> TruncateNumber;
    false -> TruncateNumber + 1
  end.

sum(List) -> lists:foldl(fun(Item, Sum) -> Sum + Item end, 0, List).

avg(List) -> sum(List) / length(List).

%%====================================================================
%% Internal functions
%%====================================================================
