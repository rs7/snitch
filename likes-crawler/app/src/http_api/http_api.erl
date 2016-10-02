-module(http_api).

-behaviour(elli_handler).

%%% behaviour
-export([handle/2, handle_event/3]).

%%%===================================================================
%%% behaviour
%%%===================================================================

handle(Req, _Args) -> handle(elli_request:method(Req), elli_request:path(Req), Req).

handle_event(_Event, _Data, _Args) -> ok.

%%%===================================================================
%%% internal
%%%===================================================================

handle('GET',[<<"metrics">>], _Req) ->
  Content = metrics:prometheus(),
  {ok, [], Content};

handle(_Method, _Path, _Req) -> {404, [], <<"Not Found">>}.
