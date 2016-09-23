-module(get_likes).

-behaviour(request_type).

%%% behaviour
-export([request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

request([OwnerId, PhotoId, 0]) ->
  {
    'likes.getList',
    #{
      type => photo,
      owner_id => OwnerId,
      item_id => PhotoId,
      count => 1000,
      v => '5.53'
    }
  };

request([OwnerId, PhotoId, Offset]) ->
  {
    'likes.getList',
    #{
      type => photo,
      owner_id => OwnerId,
      item_id => PhotoId,
      count => 1000,
      offset => Offset,
      v => '5.53'
    }
  }.

response({response, #{<<"items">> := Likers}}, [OwnerId, PhotoId, _Offset]) ->
  {
    [],
    [
      {save_job, {Liker, OwnerId, PhotoId}}
      ||
      Liker <- Likers, Liker =:= test:which_search() %todo
    ]
  }.
