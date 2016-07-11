-module(vk_user).

-include("vk_request.hrl").

-define(DEACTIVATED_KEY, <<"deactivated">>).
-define(ID_KEY, <<"id">>).

%% API exports
-export([getActive/1]).

%%====================================================================
%% API functions
%%====================================================================

getActive(Users) ->
  Response = vk_call:call(usersRequest(Users), post),
  [maps:get(?ID_KEY, User) || User <- Response, not maps:is_key(?DEACTIVATED_KEY, User)].

%%====================================================================
%% Internal functions
%%====================================================================

usersRequest(Users) -> #request{
  method = 'users.get',
  params = #{
    user_ids => string:join([integer_to_list(User) || User <- Users], ","),
    v => '5.52'
  }
}.
