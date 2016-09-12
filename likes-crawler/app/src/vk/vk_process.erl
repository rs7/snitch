-module(vk_process).

%%% api
-export([process/2]).

%%%===================================================================
%%% api
%%%===================================================================

process(Call, {user_list, UserList}) -> [{user, User} || User <- process_user_list(Call, UserList)];

process(_Call, {user, User}) -> [{album, Album} || Album <- process_user(User)];

process(Call, {album, Album}) -> [{photo, Photo} || Photo <- process_album(Call, Album)];

process(Call, {photo, Photo}) -> [{like, Like} || Like <- process_photo(Call, Photo)].

%%%===================================================================
%%% internal
%%%===================================================================

process_user_list(Call, UserList) ->
  {response, Response} = vk_heap:get(Call, filter_active_request_data(), user_ids, 1000, UserList),
  [maps:get(<<"id">>, User) || User <- Response, not maps:is_key(<<"deactivated">>, User)].

process_user(User) -> [{User, Album} || Album <- [profile, wall]].

process_album(Call, Album) ->
  {response, Response} = vk_list:get(Call, album_request_data(Album)),
  [
    {{Owner, Id}, Count}
    ||
    #{<<"id">> := Id, <<"owner_id">> := Owner, <<"likes">> := #{<<"count">> := Count}} <- Response,
    Count > 0
  ].

process_photo(Call, {Photo, LikeCount}) ->
  {response, Response} = vk_list:get(Call, likes_request_data(Photo), LikeCount),
  Response.

%%%===================================================================
%%% request_data
%%%===================================================================

filter_active_request_data() -> {
  'users.get',
  #{
    v => '5.53'
  }
}.

album_request_data({Owner, Album}) -> {
  'photos.get',
  #{
    owner_id => Owner,
    album_id => Album,
    extended => 1,
    v => '5.53'
  }
}.

likes_request_data({Owner, Photo}) -> {
  'likes.getList',
  #{
    type => photo,
    owner_id => Owner,
    item_id => Photo,
    v => '5.53'
  }
}.
