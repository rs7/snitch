-module(requester_stream).

-behaviour(gen_server).

%%% api
-export([start_link/3, status/2, data/2, fin/1, error/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(NAME(StreamRef), {via, identifiable, {?MODULE, StreamRef}}).

-record(state, {requester_id, request_id, status, data}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(StreamRef, RequesterId, RequestId) ->
  gen_server:start_link(?NAME(StreamRef), ?MODULE, {RequesterId, RequestId}, []).

status(StreamRef, Status) -> gen_server:cast(?NAME(StreamRef), {status, Status}).

data(StreamRef, Data) -> gen_server:cast(?NAME(StreamRef), {data, Data}).

fin(StreamRef) -> gen_server:cast(?NAME(StreamRef), fin).

error(StreamRef, Reason) -> gen_server:cast(?NAME(StreamRef), {error, Reason}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({RequesterId, RequestId}) ->
  NewState = #state{requester_id = RequesterId, request_id = RequestId},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({status, 200 = Status}, State) ->
  NewState = State#state{status = Status, data = <<>>},
  {noreply, NewState};

handle_cast({status, Non200Status}, State) ->
  NewState = State#state{status = Non200Status},
  fail({http_status_error, Non200Status}, NewState),
  {noreply, NewState};

handle_cast({data, Data}, #state{status = 200, data = Accumulator} = State) ->
  NewState = State#state{data = <<Accumulator/binary, Data/binary>>},
  {noreply, NewState};

handle_cast({data, _Data}, #state{status = _Non200Status} = State) -> {noreply, State};

handle_cast(fin, #state{status = 200} = State) ->
  send_decode_result(State),
  {stop, normal, State};

handle_cast(fin, #state{status = _Non200Status} = State) -> {stop, normal, State};

handle_cast({error, Reason}, State) ->
  fail(Reason, State),
  {stop, normal, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

fail(Reason, #state{requester_id = RequesterId, request_id = RequestId}) ->
  requester_queue:retry(RequesterId, RequestId).

success(Result, #state{requester_id = RequesterId, request_id = RequestId}) ->
  requester_queue:ok(RequesterId, RequestId, Result).

send_decode_result(#state{data = Data} = State) ->
  case response_lib:decode_body(Data) of
    {ok, {response, Response}} -> success({response, Response}, State);
    {ok, {error, 10}} -> fail({vk_error, 10}, State);
    {ok, {error, 1}} -> fail({vk_error, 1}, State);
    {ok, {error, ErrorCode}} -> success({error, ErrorCode}, State);
    {error, Reason} -> fail({decode_error, Reason}, State)
  end.
