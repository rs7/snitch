-module(requester_proc).

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
  {Ids, Requests} = lists:unzip(get_requests(Id)),
  Send = request:create(Requests),
  Recv = socket:process(Send),
  Responses = response:parse(Recv, length(Requests)),

  lists:foreach(fun (Argument) -> process(Id, Argument) end, lists:zip3(Ids, Requests, Responses)),

  loop(Id).

get_requests(Id) ->
  {ok, Result} = requester_queue:get(Id),
  case Result of
    [] ->
      timer:sleep(100),
      get_requests(Id);
    Requests -> Requests
  end.

process(Id, {RequestId, _Request, {ok, Result}}) -> requester_queue:reply(Id, RequestId, Result);
process(Id, {RequestId, Request, {error, Reason}}) -> requester_queue:reject(Id, RequestId, Request, Reason).
