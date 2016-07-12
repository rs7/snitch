-module(vk_photo).

-include("vk_request.hrl").

-define(ALBUMS, [profile, wall]).

%% API exports
-export([get/1, getAlbum/1, getCount/1]).

%%====================================================================
%% API functions
%%====================================================================

get(Owner) ->
  Items = lists:concat(
    rpc:parallel_eval([
      {?MODULE, getAlbum, [{Owner, Album}]} || Album <- ?ALBUMS
    ])
  ),
  [photoObject(Item) || Item <- Items].

getAlbum(AlbumObject) ->
  Request = photosRequest(AlbumObject),
  Count = vk_list:getItemCount(Request),
  PagesCount = vk_list:getPageCount(Count),
  vk_list:getPages(Request, 1, PagesCount).

getCount(Owner) ->
  Counts = rpc:parallel_eval([
    {vk_list, getItemCount, [photosRequest({Owner, Album})]} || Album <- ?ALBUMS
  ]),
  util:sum(Counts).

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
