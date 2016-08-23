-module(vk_api).

%%% api
-export([get_likes/2, filter_users/2, is_user_active/2]).

%%%===================================================================
%%% api
%%%===================================================================

filter_users(Users, Call) -> vk_api_users:filter_active(Users, Call).

get_likes(Owner, Call) -> vk_api_likes:get(Owner, Call).

is_user_active(User, Call) -> vk_api_users:is_active(User, Call).
