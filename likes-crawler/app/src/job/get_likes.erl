-module(get_likes).

%%% api
-export([request/1, response/2]).

%%%===================================================================
%%% api
%%%===================================================================

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
  [
    {save_job, {Liker, OwnerId, PhotoId}}
    ||
    Liker <- Likers, Liker =:= 1
  ].
