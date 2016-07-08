-module(vk_photo).

-include("vk_request.hrl").

%% API exports
-export([get/1, getAlbum/1]).

%%====================================================================
%% API functions
%%====================================================================

get(Owner) ->
  Items = lists:concat(
    rpc:parallel_eval([
      {?MODULE, getAlbum, [{Owner, profile}]},
      {?MODULE, getAlbum, [{Owner, wall}]}
    ])
  ),
  [photoObject(Item) || Item <- Items].

getAlbum(AlbumObject) ->
  Request = photosRequest(AlbumObject),
  Count = vk_list:getItemCount(Request),
  PagesCount = vk_list:getPageCount(Count),
  vk_list:getPages(Request, 1, PagesCount).

%%====================================================================
%% Internal functions
%%====================================================================

photoObject(
  #{
    <<"id">> := Item,
    <<"owner_id">> := Owner,
    <<"likes">> := #{
      <<"count">> := Count
    }
  }
) -> [
  {Owner, Item}, Count
].

photosRequest({Owner, Album}) -> #request{
  method = 'photos.get',
  params = #{
    owner_id => Owner,
    album_id => Album,
    extended => 1,
    v => '5.52'
  }
}.
