-module(vk_list).

-define(COUNT_KEY, <<"count">>).
-define(ITEMS_KEY, <<"items">>).
-define(LIMIT, 1000).

%% API exports
-export([getAll/1, getAll/2, getItemCount/1]).

%%====================================================================
%% API functions
%%====================================================================

getAll(Request) -> getAll(Request, getItemCount(Request)).

getAll(Request, Count) -> getPages(Request, lists:seq(1, getPageCount(Count))).

getItemCount(Request) -> getCount(vk_call:call(countRequest(Request))).

%%====================================================================
%% Internal functions
%%====================================================================

getPages(Request, Pages) ->
  Requests = [pageRequest(Request, Page) || Page <- Pages],
  Responses = vk_call:callAll(Requests),
  mergeResponses(Responses).

getPageCount(ItemsCount) -> util:ceil(ItemsCount / ?LIMIT).

getItems(Response) -> maps:get(?ITEMS_KEY, Response).

getCount(Response) -> maps:get(?COUNT_KEY, Response).

mergeResponses(Responses) -> lists:concat(lists:map(fun getItems/1, Responses)).

offset(Page) -> (Page - 1) * ?LIMIT.

pageRequest({Method, Params}, Page) -> {
  Method,
  Params#{
    count => ?LIMIT,
    offset => offset(Page)
  }
}.

countRequest({Method, Params}) -> {
  Method,
  Params#{
    count => 0
  }
}.
