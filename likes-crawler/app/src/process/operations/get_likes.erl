-module(get_likes).

%%% api
-export([request/2, response/3]).

%%%===================================================================
%%% api
%%%===================================================================

request([OwnerId, PhotoId, Offset], _ProcessContext) ->
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

response({response, #{<<"items">> := Likers}}, [OwnerId, PhotoId, _Offset], [Targeted]) ->
  [
    {out, [Liker, OwnerId, PhotoId]}
    ||
    Liker <- Likers, is_targeted(Liker, Targeted)
  ].

%%%===================================================================
%%% internal
%%%===================================================================

is_targeted(Liker, Targeted) -> ordsets:is_element(Liker, Targeted).
