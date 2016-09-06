-module(vk_list).

-define(COUNT_KEY, <<"count">>).
-define(ITEMS_KEY, <<"items">>).
-define(LIMIT, 1000).

%%% api
-export([get/2, get/3, get_count/2]).

%%%===================================================================
%%% api
%%%===================================================================

get(Call, RequestData) ->
  case Call(page_request_data(RequestData, 1)) of
    {response, #{?COUNT_KEY := 0}} ->
      {response, []};

    {response, #{?COUNT_KEY := ItemsCount, ?ITEMS_KEY := Items}} when ItemsCount =< ?LIMIT ->
      {response, Items};

    {response, #{?COUNT_KEY := ItemsCount, ?ITEMS_KEY := Items}} ->
      Pages = get_pages(Call, RequestData, pages_from(2, ItemsCount)),
      merge_pages(Pages, Items);

    {error, ErrorCode} -> {error, ErrorCode}
  end.

get(Call, RequestData, ItemsCount) ->
  Pages = get_pages(Call, RequestData, pages_from(1, ItemsCount)),
  merge_pages(Pages, []).

get_count(Call, RequestData) ->
  {response, #{?COUNT_KEY := Count}} = Call(count_request_data(RequestData)),
  Count.

%%%===================================================================
%%% internal
%%%===================================================================

pages_from(From, ItemsCount) -> lists:seq(From, get_page_count(ItemsCount)).

get_pages(Call, Request, Pages) ->
  Requests = [page_request_data(Request, Page) || Page <- Pages],
  Call(Requests).

get_page_count(ItemsCount) -> util:ceil(ItemsCount / ?LIMIT).

merge_pages([{response, #{?ITEMS_KEY := Items}} | Responses], AccItems) ->
  merge_pages(Responses, [Items | AccItems]);

merge_pages([], AccItems) -> {response, lists:flatten(lists:reverse(AccItems))};

merge_pages([{error, ErrorCode} | _Responses], _AccItems) -> {error, ErrorCode}.

%%%===================================================================
%%% requests
%%%===================================================================

offset(Page) -> (Page - 1) * ?LIMIT.

page_request_data({Method, Params}, Page) -> {Method, page_request_params(Params, offset(Page))}.

page_request_params(Params, 0) -> Params#{count => ?LIMIT};

page_request_params(Params, Offset) -> Params#{count => ?LIMIT, offset => Offset}.

count_request_data({Method, Params}) -> {
  Method,
  Params#{
    count => 0
  }
}.
