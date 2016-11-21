-module(get_albums).

-behaviour(gen_task).
-behaviour(gen_request_task).

%%% behaviour
-export([type/0, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> request.

request(OwnerId) ->
  {
    'photos.getAlbums',
    #{
      owner_id => OwnerId,
      need_system => 1,
      v => '5.53'
    }
  }.

%% пользователь удалил страницу
response({error, 15}, _Context) -> [];

response({response, #{<<"items">> := AlbumItems}}, _Context) ->
  lists:append(
    [
      [
        {get_photos, {AlbumOwnerId, AlbumId, PhotosPageOffset, PhotosPageCount}}
        ||
        {PhotosPageOffset, PhotosPageCount} <- list:params(PhotosCount, 1000)
      ]
      ||
      #{<<"id">> := AlbumId, <<"owner_id">> := AlbumOwnerId, <<"size">> := PhotosCount} <- AlbumItems,
      PhotosCount > 0
    ]
  ).
