-module(filter_users).

%%% api
-export([request/2, response/3]).

%%%===================================================================
%%% api
%%%===================================================================

request([UserList], _ProcessContext) ->
  {
    'users.get',
    #{
      user_ids => UserList,
      v => '5.53'
    }
  }.

response({response, Response}, _RequestContext, _ProcessContext) ->
  [
    {get_albums, [maps:get(<<"id">>, User)]}
    ||
    User <- Response, not maps:is_key(<<"deactivated">>, User)
  ].
