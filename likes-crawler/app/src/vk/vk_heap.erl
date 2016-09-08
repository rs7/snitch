-module(vk_heap).

%%% api
-export([get/5, get/6]).

%%%===================================================================
%%% api
%%%===================================================================

get(Call, RequestData, HeapParam, Limit, Heap) ->
  Requests = [partition_request_data(RequestData, HeapParam, HeapPart) || HeapPart <- partition_heap(Limit, Heap)],
  Responses = Call(Requests),
  process_responses(Responses, []).

get(Call, RequestData, HeapParam, Limit, Heap, check_missing) ->
  {Requests, Lengths} = lists:unzip([
    {partition_request_data(RequestData, HeapParam, HeapPart), length(HeapPart)}
    || HeapPart <- partition_heap(Limit, Heap)
  ]),
  Responses = Call(Requests),
  process_responses(Responses, [], Lengths).

%%%===================================================================
%%% internal
%%%===================================================================

process_responses(
  [{response, Items} | _RemainingResponses], _ItemsAccumulator, [Length | _RemainingLengths]
) when length(Items) =/= Length -> {error, item_missing};

process_responses(
  [{response, Items} | RemainingResponses], ItemsAccumulator, [_Length | RemainingLengths]
) -> process_responses(RemainingResponses, [Items | ItemsAccumulator], RemainingLengths);

process_responses([{error, ErrorCode} | _RemainingResponses], _ItemsAccumulator, _Lengths) -> {error, ErrorCode};

process_responses([], ItemsAccumulator, []) -> {response, lists:flatten(lists:reverse(ItemsAccumulator))}.

process_responses([{response, Items} | RemainingResponses], ItemsAccumulator) ->
  process_responses(RemainingResponses, [Items | ItemsAccumulator]);

process_responses([{error, ErrorCode} | _RemainingResponses], _ItemsAccumulator) -> {error, ErrorCode};

process_responses([], ItemsAccumulator) -> {response, lists:flatten(lists:reverse(ItemsAccumulator))}.

partition_heap(_Limit, []) -> [];

partition_heap(Limit, Heap) when length(Heap) =< Limit -> [Heap];

partition_heap(Limit, Heap) ->
  {Part, RemainingHeap} = lists:split(Limit, Heap),
  [Part | partition_heap(Limit, RemainingHeap)].

partition_request_data({Method, Params}, HeapParam, HeapPart) -> {
  Method,
  Params#{
    HeapParam => HeapPart
  }
}.
