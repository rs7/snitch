-module(vk_user).

-include("vk_request.hrl").

-define(DEACTIVATED_KEY, <<"deactivated">>).
-define(ID_KEY, <<"id">>).
-define(LIMIT, 1000).

%% API exports
-export([getActive/1, partition/1]).

%%====================================================================
%% API functions
%%====================================================================

getActive(Users) ->
  Response = lists:concat(
    rpc:parallel_eval([
      {vk_call, call, [usersRequest(UsersPart), post]} || UsersPart <- partition(Users)
    ])
  ),
  [maps:get(?ID_KEY, User) || User <- Response, not maps:is_key(?DEACTIVATED_KEY, User)].

%%====================================================================
%% Internal functions
%%====================================================================

partition([]) -> [];
partition(Users) when length(Users) =< ?LIMIT -> [Users];
partition(Users) -> {H, T} = lists:split(?LIMIT, Users), [H | partition(T)].

usersRequest(Users) -> #request{
  method = 'users.get',
  params = #{
    user_ids => string:join([integer_to_list(User) || User <- Users], ","),
    v => '5.52'
  }
}.
