-module(vk_util).

%% API
-export([ceil/1]).

%%====================================================================
%% API
%%====================================================================

ceil(Number) ->
  TruncateNumber = trunc(Number),
  case TruncateNumber == Number of
    true -> TruncateNumber;
    false -> TruncateNumber + 1
  end.

%%====================================================================
%% internal
%%====================================================================
