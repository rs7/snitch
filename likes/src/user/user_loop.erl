-module(user_loop).

%%% api
-export([start_link/1]).

%%% internal
-export([loop/1]).

%%%===================================================================
%%% api
%%%===================================================================

start_link(Id) -> {ok, spawn_link(?MODULE, loop, [Id])}.

%%%===================================================================
%%% internal
%%%===================================================================

loop(Id) ->
  {UserId, User} = get100(Id),
  ok = task:process({get_albums, User}),
  user_queue:confirm(Id, UserId),
  loop(Id).

get100(Id) ->
  case user_queue:get(Id) of
    {ok, undefined} ->
      timer:sleep(1000),
      get100(Id);

    {ok, {UserId, User}} -> {UserId, User}
  end.
