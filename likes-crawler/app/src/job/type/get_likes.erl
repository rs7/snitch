-module(get_likes).

-behaviour(gen_job).
-behaviour(gen_request_job).

%%% behaviour
-export([process/2, request/1, response/2]).

%%%===================================================================
%%% behaviour
%%%===================================================================

process(Priority, Context) -> gen_request_job:process(?MODULE, Priority, Context).

request({OwnerId, PhotoId, Offset}) ->
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

%% пользователь удалил страницу
%% пользователь удалил альбом
%% пользователь удалил фотографию
%% пользователь скрыл альбом
response({error, 15}, _Context) -> [];

response({response, #{<<"items">> := Likers}}, {OwnerId, PhotoId, _Offset}) ->
  [
    {save, {Liker, OwnerId, PhotoId}}
    ||
    Liker <- Likers, conveyor_controller:is_target_user(Liker) %temp
  ].
