-module(get_albums).

%%% api
-export([request/2, response/3]).

%%%===================================================================
%%% api
%%%===================================================================

request([OwnerId], _ProcessContext) ->
  {
    'photos.getAlbums',
    #{
      owner_id => OwnerId,
      need_system => 1,
      v => '5.53'
    }
  }.

response({response, #{<<"items">> := AlbumItems}}, _RequestContext, _ProcessContext) ->
  util:flatten(
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
