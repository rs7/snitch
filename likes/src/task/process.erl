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
  Response = call(Request),
  Children = Type:response(Response, Args),
  children(Children);

process(query, Type, Args) ->
  Query = Type:query(Args),
  Result = db:run(Query),
  Children = Type:result(Result, Args),
  children(Children).

call(Request) ->
  case vk:call(Request) of

    {ok, Result} -> Result;

    {error, Reason} ->
      io:format("error ~p~n", [Reason]),
      call(Request)

  end.

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
