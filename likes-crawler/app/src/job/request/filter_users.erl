-module(filter_users).

%%% api
-export([request/1, response/2]).

%%%===================================================================
%%% api
%%%===================================================================

request([UserList]) ->
  {
    'users.get',
    #{
      user_ids => UserList,
      v => '5.53'
    }
  }.

response({response, Response}, _RequestContext) ->
  [
    {get_albums, [maps:get(<<"id">>, User)]}
    ||
    User <- Response, not maps:is_key(<<"deactivated">>, User)
  ].
