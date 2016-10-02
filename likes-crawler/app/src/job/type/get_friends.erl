-module(get_friends).

-behaviour(gen_job).
-behaviour(gen_request_job).

%%% behaviour
-export([process/2, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

process(Priority, Context) -> gen_request_job:process(?MODULE, Priority, Context).

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
