-module(get_photos).

%%% api
-export([request/1, response/2]).

%%%===================================================================
%%% api
%%%===================================================================

request([AlbumOwnerId, AlbumId, PhotosOffset]) ->
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

response({response, #{<<"items">> := Items}}, _RequestContext) ->
  util:flatten(
    [
      [
        {get_likes, [PhotoOwnerId, PhotoId, LikesOffset]}
        ||
        LikesOffset <- util:get_offsets(0, LikesCount)
      ]
      ||
      #{<<"id">> := PhotoId, <<"owner_id">> := PhotoOwnerId, <<"likes">> := #{<<"count">> := LikesCount}} <- Items,
      LikesCount > 0
    ]
  ).
