-module(vk_api).

-include("vk_request.hrl").

%% API exports
-export([getLikes/1, getLikes/2]).

%%====================================================================
%% API functions
%%====================================================================

getLikes(LikeItem) -> vk_list:getAll(likesRequest(LikeItem)).

getLikes(LikeItem, Count) -> vk_list:getAll(likesRequest(LikeItem), Count).

%%====================================================================
%% Internal functions
%%====================================================================

likesRequest({Type, Owner, Item}) -> #request{
  method = 'likes.getList',
  params = #{
    type => Type,
    owner_id => Owner,
    item_id => Item,
    v => '5.52'
  }
}.
