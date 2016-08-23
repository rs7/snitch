-module(vk_list).

-define(LIMIT, 1000).

%%% api
-export([get/2, get/3, get_count/2]).

%%%===================================================================
%%% api
%%%===================================================================

get(Request, Call) -> get(Request, get_count(Request, Call), Call).

get(Request, Count, Call) -> get_pages(Request, lists:seq(1, get_page_count(Count)), Call).

get_count(Request, Call) -> extract_count(Call(count_request(Request))).

%%%===================================================================
%%% internal
%%%===================================================================

get_pages(Request, Pages, Call) ->
  Requests = [page_request(Request, Page) || Page <- Pages],
  merge_responses(lists:map(Call, Requests)).

get_page_count(ItemsCount) -> util:ceil(ItemsCount / ?LIMIT).

extract_items(Response) -> maps:get(<<"items">>, Response).

extract_count(Response) -> maps:get(<<"count">>, Response).

merge_responses(Responses) -> lists:concat(lists:map(fun extract_items/1, Responses)).

offset(Page) -> (Page - 1) * ?LIMIT.

page_request({Method, Params}, Page) -> {
  Method,
  Params#{
    count => ?LIMIT,
    offset => offset(Page)
  }
}.

count_request({Method, Params}) -> {
  Method,
  Params#{
    count => 0
  }
}.
