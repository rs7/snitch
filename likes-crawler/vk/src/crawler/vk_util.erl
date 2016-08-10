-module(vk_util).

%% api
-export([ceil/1]).

%%====================================================================
%% api
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
