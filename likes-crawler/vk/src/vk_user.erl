-module(vk_user).

-define(LIMIT, 1000).

%% API
-export([get/2]).

%%====================================================================
%% API
%%====================================================================

get(Users, Params) -> mergeResponses(vk:multi(createRequests(Users, Params))).

%%====================================================================
%% internal
%%====================================================================

createRequests(Users, Params) -> [partitionRequest(PartitionUsers, Params) || PartitionUsers <- partition(Users)].

mergeResponses(Responses) -> lists:concat(Responses).

partition([]) -> [];
partition(Users) when length(Users) =< ?LIMIT -> [Users];
partition(Users) -> {H, T} = lists:split(?LIMIT, Users), [H | partition(T)].

partitionRequest(Users, Params) -> {
  'users.get',
  Params#{
    user_ids => Users
  }
}.
