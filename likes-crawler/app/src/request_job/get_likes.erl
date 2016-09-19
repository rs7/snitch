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
    save([Liker, OwnerId, PhotoId])
    ||
    Liker <- Likers, Liker =:= 1
  ],
  [].

%%%===================================================================
%%% internal
%%%===================================================================

%save(_) -> ok.
save([Liker, OwnerId, PhotoId]) -> lager:info("id~B photo~B_~B", [Liker, OwnerId, PhotoId]).
