-module(vk_like).

-include("vk_request.hrl").

%% API exports
-export([get/2]).

%%====================================================================
%% API functions
%%====================================================================

get(_, 0) -> [];
get(PhotoObject, Count) ->
  PagesCount = vk_list:getPageCount(Count),
  vk_list:getPages(likesRequest(PhotoObject), 1, PagesCount).

%%====================================================================
%% Internal functions
%%====================================================================

likesRequest({Owner, Item}) -> #request{
  method = 'likes.getList',
  params = #{
    type => photo,
    owner_id => Owner,
    item_id => Item,
    v => '5.52'
  }
}.
