-module(crawler).

%% API
-export([getLikes/1, filterActive/1]).

%%====================================================================
%% API
%%====================================================================

filterActive(Users) -> crawler_user:filterActive(Users).

getLikes(Owner) ->
  {Photos, Counts} = crawler_photo:getPhotosWithLikesCounts(Owner),
  Likes = rpc:parallel_eval([
    {crawler_like, get, [Photo, Count]} || {Photo, Count} <- lists:zip(Photos, Counts)
  ]),
  lists:zip(Photos, Likes).

%%====================================================================
%% internal
%%====================================================================
