-module(vk_api_likes).

%%% api
-export([get/2]).

%%%===================================================================
%%% api
%%%===================================================================

get(Owner, Call) ->
  {Photos, Counts} = vk_api_photos:get_with_like_count(Owner, Call),
  Likes = [get_photo_likes(Photo, LikeCount, Call) || {Photo, LikeCount} <- lists:zip(Photos, Counts)],
  lists:zip(Photos, Likes).

%%%===================================================================
%%% internal
%%%===================================================================

get_photo_likes(Photo, LikeCount, Call) -> vk_list:get(likes_request(Photo), LikeCount, Call).

likes_request({Owner, Item}) -> {
  'likes.getList',
  #{
    type => photo,
    owner_id => Owner,
    item_id => Item,
    v => '5.52'
  }
}.
