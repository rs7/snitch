-module(get_friends).

-behaviour(gen_task).
-behaviour(gen_request_task).

%%% behaviour
-export([type/0, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> request.

request(User) ->
  Params =
    #{
      user_id => User,
      v => '5.60'
    },

  {'friends.get', Params}.

%% пользователь удалил страницу
response({error, 15}, _Context) -> [];

response({response, Friends}, User) -> {save_friends, {User, Friends}}.
