-module(process_lib).

%%% api
-export([]).

%%% internal
-export([process/2]).

%%%===================================================================
%%% api
%%%===================================================================

process(Call, {filter_users, UserList}) ->
  Request = {
    'users.get',
    #{
      user_ids => UserList,
      v => '5.53'
    }
  },
  {response, Response} = Call(Request),
  [
    {get_albums, maps:get(<<"id">>, User)}
    ||
    User <- Response, not maps:is_key(<<"deactivated">>, User)
  ];

process(Call, {get_albums, OwnerId}) ->
  Request = {
    'photos.getAlbums',
    #{
      owner_id => OwnerId,
      need_system => 1,
      v => '5.53'
    }
  },
  {response, #{<<"items">> := AlbumItems}} = Call(Request),
  flatten(
    [
      [
        {get_photos, OwnerId, AlbumId, PhotosOffset}
        ||
        PhotosOffset <- get_offsets(0, PhotosCount)
      ]
      ||
      #{<<"id">> := AlbumId, <<"size">> := PhotosCount} <- AlbumItems,
      PhotosCount > 0
    ]
  );

process(Call, {get_photos, AlbumOwnerId, AlbumId, PhotosOffset}) ->
  Request = {
    'photos.get',
    #{
      owner_id => AlbumOwnerId,
      album_id => AlbumId,
      extended => 1,
      count => 1000,
      offset => PhotosOffset,
      v => '5.53'
    }
  },
  {response, #{<<"items">> := Items}} = Call(Request),
  flatten(
    [
      [
        {get_likes, PhotoOwnerId, PhotoId, LikesOffset}
        ||
        LikesOffset <- get_offsets(0, LikesCount)
      ]
      ||
      #{<<"id">> := PhotoId, <<"owner_id">> := PhotoOwnerId, <<"likes">> := #{<<"count">> := LikesCount}} <- Items,
      LikesCount > 0
    ]
  );

process(Call, {get_likes, OwnerId, PhotoId, Offset}) ->
  Request = {
    'likes.getList',
    #{
      type => photo,
      owner_id => OwnerId,
      item_id => PhotoId,
      count => 1000,
      offset => Offset,
      v => '5.53'
    }
  },
  {response, #{<<"items">> := Likers}} = Call(Request),
  [
    save(Liker, OwnerId, PhotoId)
    ||
    Liker <- Likers, is_targeted(Liker)
  ],
  [].

is_targeted(_Liker) -> true;
is_targeted(Liker) -> ordsets:is_element(Liker, ordsets:from_list([1, 2, 3])).

save(Liker, OwnerId, PhotoId) -> lager:info("id~B photo~B_~B", [Liker, OwnerId, PhotoId]).

%%%===================================================================
%%% internal
%%%===================================================================

get_offsets(From, To) when From > To -> [];

get_offsets(From, To) -> [From | get_offsets(From + 1000, To)].

flatten(List) -> flatten(lists:reverse(List), []).
flatten([], Acc) -> Acc;
flatten([H | T], Acc) -> flatten(T, H ++ Acc).
