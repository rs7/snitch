-module(get_photos).

-behaviour(gen_task).
-behaviour(gen_request_task).

%%% behaviour
-export([type/0, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> request.

request({Owner, Album, Offset, Count}) ->
  Params =
    #{
      owner_id => Owner,
      album_id => Album,
      extended => 1,
      v => '5.53'
    },

  FinalParams = list:add_page_params(Params, Offset, Count, 1000),

  {'photos.get', FinalParams}.

%% пользователь скрыл альбом
response({error, 7}, _Context) -> [];

%% пользователь удалил страницу
response({error, 15}, _Context) -> [];

%% пользователь удалил альбом
response({error, 200}, _Context) -> [];

response({response, #{<<"items">> := Items}}, _Context) ->
  lists:append(
    [
      [
        {get_likes, {Owner, Photo, Offset, Count}}
        ||
        {Offset, Count} <- list:partition(LikesCount, 1000)
      ]
      ||
      #{<<"id">> := Photo, <<"owner_id">> := Owner, <<"likes">> := #{<<"count">> := LikesCount}} <- Items,
      LikesCount > 0
    ]
  ).
