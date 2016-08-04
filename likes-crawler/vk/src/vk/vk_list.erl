-module(vk_list).

-define(LIMIT, 1000).

%% API
-export([get/1, get/2, getCount/1]).

%%====================================================================
%% API
%%====================================================================

get(Request) -> get(Request, getCount(Request)).

get(Request, Count) -> getPages(Request, lists:seq(1, getPageCount(Count))).

getCount(Request) -> extractCount(vk:single(countRequest(Request))).

%%====================================================================
%% internal
%%====================================================================

getPages(Request, Pages) ->
  Requests = [pageRequest(Request, Page) || Page <- Pages],
  Responses = vk:multi(Requests),
  mergeResponses(Responses).

getPageCount(ItemsCount) -> vk_util:ceil(ItemsCount / ?LIMIT).

extractItems(Response) -> maps:get(<<"items">>, Response).

extractCount(Response) -> maps:get(<<"count">>, Response).

mergeResponses(Responses) -> lists:concat(lists:map(fun extractItems/1, Responses)).

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
