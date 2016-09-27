-module(get_albums).

-behaviour(gen_job).
-behaviour(gen_request_job).

%%% behaviour
-export([process/2, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

process(Priority, Context) -> gen_request_job:process(?MODULE, Priority, Context).

request(OwnerId) ->
  {
    'photos.getAlbums',
    #{
      owner_id => OwnerId,
      need_system => 1,
      v => '5.53'
    }
  }.

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
