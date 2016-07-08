-module(vk).

%% API exports
-export([getLikes/1, photoLikes/1]).

%%====================================================================
%% API functions
%%====================================================================

getLikes(Owner) -> rpc:pmap({?MODULE, photoLikes}, [], vk_photo:get(Owner)).

photoLikes([PhotoObject, Count]) -> {PhotoObject, vk_like:get(PhotoObject, Count)}.

%%====================================================================
%% Internal functions
%%====================================================================
