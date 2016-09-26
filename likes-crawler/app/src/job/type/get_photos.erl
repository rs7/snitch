-module(get_photos).

-behaviour(gen_job).
-behaviour(gen_request_job).

%%% behaviour
-export([process/2, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

process(Priority, Context) -> gen_request_job:process(?MODULE, Priority, Context).

request({AlbumOwnerId, AlbumId, PhotosOffset}) ->
  {
    'photos.get',
    #{
      owner_id => AlbumOwnerId,
      album_id => AlbumId,
      extended => 1,
      count => 1000,
      offset => PhotosOffset,
      v => '5.53'
    }
  }.

%% пользователь скрыл альбом
response({error, 7}, _Context) -> [];

%% пользователь удалил страницу
response({error, 15}, _Context) -> [];

%% пользователь удалил альбом
response({error, 200}, _Context) -> [];

response({response, #{<<"items">> := Items}}, _Context) ->
  lists:append(
    [
      [
        {get_likes, {PhotoOwnerId, PhotoId, LikesOffset}}
        ||
        LikesOffset <- util:get_offsets(0, LikesCount)
      ]
      ||
      #{<<"id">> := PhotoId, <<"owner_id">> := PhotoOwnerId, <<"likes">> := #{<<"count">> := LikesCount}} <- Items,
      LikesCount > 0
    ]
  ).
