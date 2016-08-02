-module(vk_like).

%% API exports
-export([get/2]).

%%====================================================================
%% API functions
%%====================================================================

get(_PhotoObject, 0) -> [];
get(PhotoObject, Count) -> vk_list:get(likesRequest(PhotoObject), Count).

%%====================================================================
%% Internal functions
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
