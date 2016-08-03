-module(crawler_photo).

-define(ALBUMS, [profile, wall]).

%% API
-export([getPhotosWithLikesCounts/1, getAlbum/1, getCount/1]).

%%====================================================================
%% API
%%====================================================================

getPhotosWithLikesCounts(Owner) ->
  Items = lists:concat(rpc:pmap({?MODULE, getAlbum}, [], albums(Owner))),
  lists:unzip(lists:map(fun photoObject/1, Items)).

getAlbum(AlbumObject) -> vk_list:get(photosRequest(AlbumObject)).

getCount(Owner) ->
  Requests = lists:map(fun photosRequest/1, albums(Owner)),
  lists:sum(rpc:pmap({vk_list, getItemCount}, [], Requests)).

%%====================================================================
%% internal
%%====================================================================

albums(Owner) -> [{Owner, Album} || Album <- ?ALBUMS].

photoObject(
  #{
    <<"id">> := Item,
    <<"owner_id">> := Owner,
    <<"likes">> := #{
      <<"count">> := Count
    }
  }
) -> {
  {Owner, Item}, Count
}.

photosRequest({Owner, Album}) -> {
  'photos.get',
  #{
    owner_id => Owner,
    album_id => Album,
    extended => 1,
    v => '5.52'
  }
}.
