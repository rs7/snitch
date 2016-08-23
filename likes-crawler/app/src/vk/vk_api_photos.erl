-module(vk_api_photos).

%%% api
-export([get_with_like_count/2]).

%%%===================================================================
%%% api
%%%===================================================================

get_with_like_count(Owner, Call) -> lists:unzip(process(Owner, Call)).

%%%===================================================================
%%% internal
%%%===================================================================

process(Owner, Call) -> lists:concat([process_album(Album, Call) || Album <- get_albums(Owner)]).

process_album(Album, Call) ->
  Response = vk_list:get(album_request(Album), Call),
  [{get_photo(Item), get_like_count(Item)} || Item <- Response].

get_albums(Owner) -> [{Owner, Album} || Album <- [profile, wall]].

get_photo(#{<<"id">> := Item, <<"owner_id">> := Owner}) -> {Owner, Item}.
get_like_count(#{<<"likes">> := #{<<"count">> := Count}}) -> Count.

album_request({Owner, Album}) -> {
  'photos.get',
  #{
    owner_id => Owner,
    album_id => Album,
    extended => 1,
    v => '5.52'
  }
}.
