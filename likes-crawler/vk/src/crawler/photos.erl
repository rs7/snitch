-module(photos).

%% API
-export([getWithLikeCount/1]).

%%====================================================================
%% API
%%====================================================================

getWithLikeCount(Owner) -> lists:unzip(process(Owner)).

%%====================================================================
%% internal
%%====================================================================

process(Owner) -> lists:concat(lists:map(fun processAlbum/1, getAlbums(Owner))).

processAlbum(Album) ->
  Response = vk_list:get(albumRequest(Album)),
  [{getPhoto(Item), getLikeCount(Item)} || Item <- Response].

getAlbums(Owner) -> [{Owner, Album} || Album <- [profile, wall]].

getPhoto(#{<<"id">> := Item, <<"owner_id">> := Owner}) -> {Owner, Item}.
getLikeCount(#{<<"likes">> := #{<<"count">> := Count}}) -> Count.

albumRequest({Owner, Album}) -> {
  'photos.get',
  #{
    owner_id => Owner,
    album_id => Album,
    extended => 1,
    v => '5.52'
  }
}.
