-module(vk_list).

-include("vk_request.hrl").

-define(COUNT_KEY, <<"count">>).
-define(ITEMS_KEY, <<"items">>).
-define(LIMIT, 1000).

%% API exports
-export([getAll/1, getAll/2]).

%%====================================================================
%% API functions
%%====================================================================

getAll(Request) ->
  #{?COUNT_KEY := Count, ?ITEMS_KEY := Items} = vk_call:call(putCount(Request, ?LIMIT)),
  lists:concat([Items, getAll(Request, Count)]).

getAll(Request, Count) ->
  Results = callAll(Request, Count),
  Items = [maps:get(?ITEMS_KEY, Result) || Result <- Results],
  lists:concat(Items).

%%====================================================================
%% Internal functions
%%====================================================================

callAll(Request, Count) -> vk_call:callAll(requestList(putCount(Request, ?LIMIT), offsetList(Count))).

requestList(Request, OffsetList) -> [putOffset(Request, Offset) || Offset <- OffsetList].

offsetList(Count) -> [PageNumber * ?LIMIT || PageNumber <- lists:seq(0, util:ceil(Count / ?LIMIT) - 1)].

putCount(Request, Count) -> putParamToRequest(Request, count, Count).
putOffset(Request, Offset) -> putParamToRequest(Request, offset, Offset).

putParamToRequest(Request, Key, Value) -> Request#request{params = (Request#request.params)#{Key => Value}}.
