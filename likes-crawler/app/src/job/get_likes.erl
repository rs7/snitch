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
    save({Liker, OwnerId, PhotoId})
    ||
    Liker <- Likers, Liker =:= 1
  ],
  [].

%%%===================================================================
%%% internal
%%%===================================================================

save({Liker, OwnerId, PhotoId}) ->
  {ok, OutputFilename} = application:get_env(output_file),
  WriteFileResult = file:write_file(OutputFilename, [output_line({Liker, OwnerId, PhotoId}), $\n], [append]),
  lager:info("id~B photo~B_~B ~p", [Liker, OwnerId, PhotoId, WriteFileResult]).

output_line({Liker, OwnerId, PhotoId}) ->
  io_lib:format(
    "<a href=\"http://vk.com/id~B\">id~B</a> "
    "<a href=\"http://vk.com/id~B\">id~B</a> "
    "<a href=\"http://vk.com/photo~B_~B\">photo~B_~B</a> "
    "<br>",
    [Liker, Liker, OwnerId, OwnerId, OwnerId, PhotoId, OwnerId, PhotoId]
  ).

