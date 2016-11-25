-module(task).

%%% api
-export([process/1, ensure_process/1]).

%%%===================================================================
%%% api
%%%===================================================================

process({Type, Args}) -> process(Type:type(), Type, Args).

%%%===================================================================
%%% internal
%%%===================================================================

process(request, Type, Args) ->
  Request = Type:request(Args),
  Response = vk:call(Request),
  Children = Type:response(Response, Args),
  process_children(Children);

process(query, Type, Args) ->
  Query = Type:query(Args),
  Result = db:run(Query),
  Children = Type:result(Result, Args),
  process_children(Children).

process_children(Children) ->
  RpcResult = rpc:parallel_eval([
    {?MODULE, ensure_process, [Task]} || Task <- Children
  ]),

  case process_rpc_result(RpcResult) of
    ok -> ok;
    {error, Reason} -> process_children(Children)
  end.

ensure_process(Task) ->
  case rpc:call(node(), ?MODULE, process, [Task]) of
    {badrpc, _Reason} -> ensure_process(Task);
    ok -> ok
  end.

process_rpc_result([ok | Remaining]) -> process_rpc_result(Remaining);
process_rpc_result([{badrpc, Reason} | _]) -> {error, Reason};
process_rpc_result([]) -> ok.
