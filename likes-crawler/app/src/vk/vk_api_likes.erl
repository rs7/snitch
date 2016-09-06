-module(vk_api_likes).

%%% api
-export([get/2]).

%%% internal
-export([get_photo_likes/3]).

%%%===================================================================
%%% api
%%%===================================================================

get(Call, Owner) ->
  {Photos, Counts} = vk_api_photos:get_with_likes_count(Call, Owner),
  rpc:parallel_eval([
    {?MODULE, get_photo_likes, [Call, Photo, LikeCount]}
    ||
    {Photo, LikeCount} <- lists:zip(Photos, Counts), LikeCount > 0
  ]).

%%%===================================================================
%%% internal
%%%===================================================================

get_photo_likes(Call, Photo, LikeCount) ->
  {response, Response} = vk_list:get(Call, likes_request(Photo), LikeCount),
  {Photo, Response}.

likes_request({Owner, Item}) -> {
  'likes.getList',
  #{
    type => photo,
    owner_id => Owner,
    item_id => Item,
    v => '5.53'
  }
}.
