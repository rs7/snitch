-module(vk).

%% API exports
-export([getLikes/1, filterActive/1]).

%%====================================================================
%% API functions
%%====================================================================

filterActive(Users) -> vk_user:filterActive(Users).

getLikes(Owner) ->
  {Photos, Counts} = vk_photo:getPhotosWithLikesCounts(Owner),
  Likes = rpc:parallel_eval([
    {vk_like, get, [Photo, Count]} || {Photo, Count} <- lists:zip(Photos, Counts)
  ]),
  lists:zip(Photos, Likes).

%%====================================================================
%% Internal functions
%%====================================================================
