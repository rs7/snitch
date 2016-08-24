-module(vk_api_users).

%%% api
-export([filter_active/2, is_active/2]).

%%%===================================================================
%%% api
%%%===================================================================

filter_active(Users, Call) ->
  Response = vk_user:get(Users, #{v => '5.53'}, Call),
  [maps:get(<<"id">>, User) || User <- Response, not maps:is_key(<<"deactivated">>, User)].

is_active(User, Call) ->
  [Response] = vk_user:get([User], #{v => '5.53'}, Call),
  not maps:is_key(<<"deactivated">>, Response).
