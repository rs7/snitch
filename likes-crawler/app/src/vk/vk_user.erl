-module(vk_user).

-define(LIMIT, 1000).

%%% api
-export([get/3]).

%%%===================================================================
%%% api
%%%===================================================================

get(Users, Params, Call) -> merge_responses(lists:map(Call, create_requests(Users, Params))).

%%%===================================================================
%%% internal
%%%===================================================================

create_requests(Users, Params) -> [partition_request(PartitionUsers, Params) || PartitionUsers <- partition(Users)].

merge_responses(Responses) -> lists:concat([Response || {response, Response} <- Responses]).

partition([]) -> [];
partition(Users) when length(Users) =< ?LIMIT -> [Users];
partition(Users) -> {H, T} = lists:split(?LIMIT, Users), [H | partition(T)].

partition_request(Users, Params) -> {
  'users.get',
  Params#{
    user_ids => Users
  }
}.
