-module(vk_user).

-define(DEACTIVATED_KEY, <<"deactivated">>).
-define(ID_KEY, <<"id">>).
-define(LIMIT, 1000).

%% API exports
-export([filterActive/1]).

%%====================================================================
%% API functions
%%====================================================================

filterActive(Users) ->
  Requests = lists:map(fun getUsersRequest/1, partition(Users)),
  Response = lists:concat(vk_call:callAll(Requests)),
  [maps:get(?ID_KEY, User) || User <- Response, not maps:is_key(?DEACTIVATED_KEY, User)].

%%====================================================================
%% Internal functions
%%====================================================================

partition([]) -> [];
partition(Users) when length(Users) =< ?LIMIT -> [Users];
partition(Users) -> {H, T} = lists:split(?LIMIT, Users), [H | partition(T)].

getUsersRequest(Users) -> {
  'users.get',
  #{
    user_ids => Users,
    v => '5.52'
  }
}.
