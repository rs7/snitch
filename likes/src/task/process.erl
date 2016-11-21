-module(process).

%%% api
-export([process/1]).

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
  children(Children);

process(query, Type, Args) ->
  Query = Type:query(Args),
  Result = db:run(Query),
  Children = Type:result(Result, Args),
  children(Children).

children(Children) -> parallel(?MODULE, process, Children).

parallel(Module, Function, ArgumentList) ->
  Calls = [
    {Module, Function, [Argument]} || Argument <- ArgumentList
  ],

  Result = rpc:parallel_eval(Calls),

  process_rpc_result(Result).

process_rpc_result([ok | Remaining]) -> process_rpc_result(Remaining);
process_rpc_result([{badrpc, Reason} | _]) -> {error, Reason};
process_rpc_result([]) -> ok.
