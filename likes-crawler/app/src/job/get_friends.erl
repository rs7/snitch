-module(get_friends).

-behaviour(request_type).

%%% behaviour
-export([request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

request(User) ->
  {
    'friends.get',
    #{
      user_id => User,
      v => '5.53'
    }
  }.

%% пользователь удалил страницу
response({error, 15}, _Context) -> {[],[]};

response({response, #{<<"items">> := Friends, <<"count">> := _FriendsCount}}, _Context) ->
  {
    [
      {filter_users, FriendsPart}
      ||
      FriendsPart <- util:list_partition(Friends, 1000)
    ],
    []
  }.
