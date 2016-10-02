-module(util).

%%% api
-export([ceil/1, parallel/2, list_split/2, list_partition/2, send/2]).

%%%===================================================================
%%% api
%%%===================================================================

ceil(Number) ->
  TruncateNumber = trunc(Number),
  case TruncateNumber == Number of
    true -> TruncateNumber;
    false -> TruncateNumber + 1
  end.

parallel({Module, Function}, ArgumentsList) ->
  Result = rpc:parallel_eval([
    {Module, Function, Arguments} || Arguments <- ArgumentsList
  ]),
  case process_rpc_result(Result) of
    {error, Reason} -> {error, Reason};
    ok -> {ok, Result}
  end.

process_rpc_result([{badrpc, Reason} | _]) -> {error, Reason};
process_rpc_result([_ | Remaining]) -> process_rpc_result(Remaining);
process_rpc_result([]) -> ok.

list_split(List, Count) when length(List) >= Count -> lists:split(Count, List);
list_split(List, _Count) -> {List, []}.

list_partition(List, PartSize) when length(List) =< PartSize -> [List];
list_partition(List, PartSize) ->
  {Part, Remaining} = lists:split(PartSize, List),
  [Part | list_partition(Remaining, PartSize)].

send({via, Module, Name}, Info) -> Module:send(Name, Info);
send({global, Name}, Info) -> send({via, global, Name}, Info);
send({local, Name}, Info) -> send({via, erlang, Name}, Info);
send(Name, Info) -> send({local, Name}, Info).
