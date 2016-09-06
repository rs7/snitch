-module(vk_api_photos).

%%% api
-export([get_with_likes_count/2]).

%%% internal
-export([process_album/2]).

%%%===================================================================
%%% api
%%%===================================================================

get_with_likes_count(Call, Owner) -> lists:unzip(process(Call, Owner)).

%%%===================================================================
%%% internal
%%%===================================================================

process(Call, Owner) -> lists:concat(
  rpc:parallel_eval([
    {?MODULE, process_album, [Call, Album]} || Album <- get_albums(Owner)
  ])
).

process_album(Call, Album) ->
  {response, Response} = vk_list:get(Call, album_request_data(Album)),
  [{get_photo(Item), get_like_count(Item)} || Item <- Response].

get_albums(Owner) -> [{Owner, Album} || Album <- [profile, wall]].

get_photo(#{<<"id">> := Item, <<"owner_id">> := Owner}) -> {Owner, Item}.
get_like_count(#{<<"likes">> := #{<<"count">> := Count}}) -> Count.

album_request_data({Owner, Album}) -> {
  'photos.get',
  #{
    owner_id => Owner,
    album_id => Album,
    extended => 1,
    v => '5.53'
  }
}.
