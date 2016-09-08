-module(vk_process).

%%% api
-export([process_users/3]).

%%% internal
-export([process_user/2, process_album/2, process_photo/2]).

-define(ALBUMS, [profile, wall]).

%%%===================================================================
%%% api
%%%===================================================================

process_users(Call, Users, TargetedUsers) ->
  {response, Response} = vk_heap:get(Call, filter_active_request_data(), user_ids, 1000, Users),
  ActiveUsers = [maps:get(<<"id">>, User) || User <- Response, not maps:is_key(<<"deactivated">>, User)],

  {ok, Result} = util:parallel(
    {?MODULE, process_user},
    [[Call, User] || User <- ActiveUsers]
  ),
  process_result(lists:concat(Result), TargetedUsers).

process_user(Call, User) ->
  Albums = [{User, Album} || Album <- ?ALBUMS],

  {ok, Result} = util:parallel(
    {?MODULE, process_album},
    [[Call, Album] || Album <- Albums]
  ),
  lists:concat(Result).

process_album(Call, Album) ->
  {response, Response} = vk_list:get(Call, album_request_data(Album)),
  Photos = [
    {{User, Id}, Count}
    ||
    #{<<"id">> := Id, <<"owner_id">> := User, <<"likes">> := #{<<"count">> := Count}} <- Response, Count > 0
  ],

  {ok, Result} = util:parallel(
    {?MODULE, process_photo},
    [[Call, Photo] || Photo <- Photos]
  ),
  Result.

process_photo(Call, {Photo, LikeCount}) ->
  {response, Response} = vk_list:get(Call, likes_request_data(Photo), LikeCount),
  {Photo, Response}.

%%%===================================================================
%%% internal
%%%===================================================================

process_result(Result, TargetedUsers) when is_list(TargetedUsers) ->
  lager:debug("photos count: ~p", [length(Result)]),

  TargetDictionary = dict:from_list([{User, []} || User <- TargetedUsers]),

  maps:from_list(dict:to_list(process_result_t(Result, TargetDictionary))).

process_result_t([], TargetDictionary) -> TargetDictionary;

process_result_t([{Photo, Users} | Remaining], TargetDictionary) ->
  merge(
    process_likes(Photo, Users, TargetDictionary),
    process_result_t(Remaining, TargetDictionary)
  ).

process_likes(Photo, [User | RemainingUsers], TargetDictionary) ->
  case dict:is_key(User, TargetDictionary) of
    false -> process_likes(Photo, RemainingUsers, TargetDictionary);
    true -> process_likes(Photo, RemainingUsers, dict:append(User, Photo, TargetDictionary))
  end;

process_likes(_Photo, [], TargetDictionary) -> TargetDictionary.

merge(Dict1, Dict2) -> dict:merge(fun merge_resolver/3, Dict1, Dict2).

merge_resolver(_Key, Value1, Value2) -> lists:concat([Value1, Value2]).

%%%===================================================================
%%% request_data
%%%===================================================================

filter_active_request_data() -> {
  'users.get',
  #{
    v => '5.53'
  }
}.

album_request_data({User, Album}) -> {
  'photos.get',
  #{
    owner_id => User,
    album_id => Album,
    extended => 1,
    v => '5.53'
  }
}.

likes_request_data({User, Item}) -> {
  'likes.getList',
  #{
    type => photo,
    owner_id => User,
    item_id => Item,
    v => '5.53'
  }
}.
