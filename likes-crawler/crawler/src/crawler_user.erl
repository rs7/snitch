-module(crawler_user).

-define(LIMIT, 1000).

%% API
-export([filterActive/1]).

%%====================================================================
%% API
%%====================================================================

filterActive(Users) ->
  Requests = lists:map(fun getUsersRequest/1, partition(Users)),
  Response = lists:concat(vk:multi(Requests)),
  [maps:get(<<"id">>, User) || User <- Response, not maps:is_key(<<"deactivated">>, User)].

%%====================================================================
%% internal
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
