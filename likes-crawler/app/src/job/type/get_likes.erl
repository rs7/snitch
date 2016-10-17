-module(get_likes).

-behaviour(gen_job).
-behaviour(gen_request_job).

%%% behaviour
-export([type/0, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> request.

request({OwnerId, PhotoId, Offset, Count}) ->
  Params = maps:merge(
    #{
      type => photo,
      owner_id => OwnerId,
      item_id => PhotoId,
      v => '5.53'
    },
    list:optimize_map(Offset, Count, 100)
  ),
  {'likes.getList', Params}.

%% пользователь удалил страницу
%% пользователь удалил альбом
%% пользователь удалил фотографию
%% пользователь скрыл альбом
response({error, 15}, _Context) -> [];

response({response, #{<<"items">> := Likers}}, {OwnerId, PhotoId, _Offset, _Count}) ->
  [{save, {OwnerId, PhotoId, Likers}}].
