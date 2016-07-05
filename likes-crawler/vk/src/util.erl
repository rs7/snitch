-module(util).

%% API exports
-export([ceil/1]).

%%====================================================================
%% API functions
%%====================================================================

ceil(Number) ->
  TruncateNumber = trunc(Number),
  case TruncateNumber == Number of
    true -> TruncateNumber;
    false -> TruncateNumber + 1
  end.

%%====================================================================
%% Internal functions
%%====================================================================
