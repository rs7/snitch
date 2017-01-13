-module(get_albums).

-behaviour(gen_task).
-behaviour(gen_request_task).

%%% behaviour
-export([type/0, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

type() -> request.

request(Owner) ->
  Params =
    #{
      owner_id => Owner,
      need_system => 1,
      v => '5.60'
    },

  {'photos.getAlbums', Params}.

%% пользователь удалил страницу
response({error, 15}, _Context) -> [];

response({response, #{<<"items">> := Items}}, _Context) ->
  lists:append(
    [
      [
        {Owner, Album, Offset, Count}
        ||
        {Offset, Count} <- list:partition(Size, 1000)
      ]
      ||
      #{<<"id">> := Album, <<"owner_id">> := Owner, <<"size">> := Size} <- Items,
      Size > 0
    ]
  ).
