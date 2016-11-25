-module(get_likes).

-behaviour(gen_task).
-behaviour(gen_request_task).

%%% behaviour
-export([type/0, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> request.

request({Owner, Photo, Offset, Count}) ->
  Params =
    #{
      type => photo,
      owner_id => Owner,
      item_id => Photo,
      v => '5.53'
    },

  FinalParams = list:add_page_params(Params, Offset, Count, 100),

  {'likes.getList', FinalParams}.

%% пользователь удалил страницу
%% пользователь удалил альбом
%% пользователь удалил фотографию
%% пользователь скрыл альбом
response({error, 15}, _Context) -> [];

response({response, #{<<"items">> := Likers}}, {Owner, Photo, _Offset, _Count}) ->
  [{save_likes, {Owner, Photo, Likers}}].
