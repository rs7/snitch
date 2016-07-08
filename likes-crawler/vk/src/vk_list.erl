-module(vk_list).

-include("vk_request.hrl").

-define(COUNT_KEY, <<"count">>).
-define(ITEMS_KEY, <<"items">>).
-define(LIMIT, 1000).

%% API exports
-export([getPages/3, getPageCount/1, getItemCount/1, mergeResponses/1]).

%%====================================================================
%% API functions
%%====================================================================

getPages(Request, PageFrom, PageTo) ->
  Requests = [requestWithPageParams(Request, offset(Page)) || Page <- lists:seq(PageFrom, PageTo)],
  Responses = rpc:pmap({vk_call, call}, [], Requests),
  mergeResponses(Responses).

getPageCount(ItemsCount) -> util:ceil(ItemsCount / ?LIMIT).

getItemCount(Request) -> getCount(vk_call:call(requestWithCountParams(Request))).

%%====================================================================
%% Internal functions
%%====================================================================

getItems(Response) -> maps:get(?ITEMS_KEY, Response).

getCount(Response) -> maps:get(?COUNT_KEY, Response).

mergeResponses(Responses) -> lists:concat([getItems(Response) || Response <- Responses]).

offset(Page) -> (Page - 1) * ?LIMIT.

requestWithPageParams(Request, Offset) -> Request#request{
  params = (Request#request.params)#{
    count => ?LIMIT,
    offset => Offset
  }
}.

requestWithCountParams(Request) -> Request#request{
  params = (Request#request.params)#{
    count => 0
  }
}.
