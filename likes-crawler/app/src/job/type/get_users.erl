-module(get_users).

-behaviour(gen_job).
-behaviour(gen_request_job).

%%% behaviour
-export([type/0, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> request.

request(Users) ->
  {
    'users.get',
    #{
      user_ids => Users,
      v => '5.53'
    }
  }.

response({response, Response}, _Context) ->
  [
    {get_albums, maps:get(<<"id">>, User)}
    ||
    User <- Response, not maps:is_key(<<"deactivated">>, User)
  ].
