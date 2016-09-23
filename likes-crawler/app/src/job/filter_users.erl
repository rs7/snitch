-module(filter_users).

-behaviour(request_type).

%%% behaviour
-export([request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

request(UserList) ->
  {
    'users.get',
    #{
      user_ids => UserList,
      v => '5.53'
    }
  }.

response({response, Response}, _Context) ->
  {
    [
      {get_albums, [maps:get(<<"id">>, User)]}
      ||
      User <- Response, not maps:is_key(<<"deactivated">>, User)
    ],
    []
  }.
