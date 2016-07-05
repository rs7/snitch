-module(vk_api).

-include("vk_request.hrl").

%% API exports
-export([getPhotosLikes/2]).

%%====================================================================
%% API functions
%%====================================================================

getPhotosLikes({Owner, Photo}, Count) -> vk_list:getAll(
  #request{
    method = 'likes.getList',
    params = #{
      owner_id => Owner,
      item_id => Photo,
      type => photo,
      v => '5.52'
    }
  },
  Count
).

%%====================================================================
%% Internal functions
%%====================================================================
