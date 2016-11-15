-module(util).

%%% api
-export([ceil/1, parallel/2, send/2]).

%%%===================================================================
%%% api
%%%===================================================================

ceil(Number) ->
  TruncateNumber = trunc(Number),
  case TruncateNumber == Number of
    true -> TruncateNumber;
    false -> TruncateNumber + 1
  end.
