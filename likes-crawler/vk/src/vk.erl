-module(vk).

%% API exports
-export([getLikes/1, photoLikes/1, process/1]).

%%====================================================================
%% API functions
%%====================================================================

process(Users) ->
  ActiveUsers = vk_user:getActive(Users),
  lists:concat(rpc:pmap({?MODULE, getLikes}, [], ActiveUsers)).

getLikes(Owner) -> rpc:pmap({?MODULE, photoLikes}, [], vk_photo:get(Owner)).

photoLikes([PhotoObject, Count]) -> {PhotoObject, vk_like:get(PhotoObject, Count)}.

%%====================================================================
%% Internal functions
%%====================================================================
