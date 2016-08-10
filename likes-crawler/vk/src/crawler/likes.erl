-module(likes).

%% api
-export([get/1]).

%%====================================================================
%% api
%%====================================================================

get(Owner) ->
  {Photos, Counts} = photos:getWithLikeCount(Owner),
  Likes = [getPhotoLikes(Photo, LikeCount) || {Photo, LikeCount} <- lists:zip(Photos, Counts)],
  lists:zip(Photos, Likes).

%%====================================================================
%% internal
%%====================================================================

getPhotoLikes(Photo, LikeCount) -> vk:list(likesRequest(Photo), LikeCount).

likesRequest({Owner, Item}) -> {
  'likes.getList',
  #{
    type => photo,
    owner_id => Owner,
    item_id => Item,
    v => '5.52'
  }
}.
