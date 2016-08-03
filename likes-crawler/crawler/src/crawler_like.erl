-module(crawler_like).

%% API
-export([get/2]).

%%====================================================================
%% API
%%====================================================================

get(_PhotoObject, 0) -> [];
get(PhotoObject, Count) -> vk:list(likesRequest(PhotoObject), Count).

%%====================================================================
%% internal
%%====================================================================

likesRequest({Owner, Item}) -> {
  'likes.getList',
  #{
    type => photo,
    owner_id => Owner,
    item_id => Item,
    v => '5.52'
  }
}.
