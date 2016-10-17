-module(get_friends).

-behaviour(gen_job).
-behaviour(gen_request_job).

%%% behaviour
-export([type/0, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> request.

request(User) ->
  {
    'friends.get',
    #{
      user_id => User,
      v => '5.53'
    }
  }.

%% пользователь удалил страницу
response({error, 15}, _Context) -> [];

response({response, #{<<"items">> := Friends, <<"count">> := _FriendsCount}}, _Context) ->
  [
    {get_users, FriendsPart}
    ||
    FriendsPart <- util:list_partition(Friends, 1000)
  ].
