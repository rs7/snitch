-module(get_albums).

%%% api
-export([request/1, response/2]).

%%%===================================================================
%%% api
%%%===================================================================

request([OwnerId]) ->
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
        {get_photos, [AlbumOwnerId, AlbumId, PhotosOffset]}
        ||
        PhotosOffset <- util:get_offsets(0, PhotosCount)
      ]
      ||
      #{<<"id">> := AlbumId, <<"owner_id">> := AlbumOwnerId, <<"size">> := PhotosCount} <- AlbumItems,
      PhotosCount > 0
    ]
  ).
