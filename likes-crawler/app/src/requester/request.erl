-module(request).

-behaviour(gen_server).

%%% api
-export([start_link/2, status/3, data/3, fin/2, error/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  requester_ref,
  request_ref,
  status,
  data
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterRef, RequestRef) -> gen_server:start_link(?MODULE, {RequesterRef, RequestRef}, []).

status(RequestPid, RequestRef, Status) -> gen_server:cast(RequestPid, {status, RequestRef, Status}).

data(RequestPid, RequestRef, Data) -> gen_server:cast(RequestPid, {data, RequestRef, Data}).

fin(RequestPid, RequestRef) -> gen_server:cast(RequestPid, {fin, RequestRef}).

error(RequestPid, RequestRef, Reason) -> gen_server:cast(RequestPid, {error, RequestRef, Reason}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({RequesterRef, RequestRef}) ->
  NewState = #state{
    requester_ref = RequesterRef,
    request_ref = RequestRef
  },
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({status, RequestRef, 200 = Status}, #state{request_ref = RequestRef} = State) ->
  NewState = State#state{status = Status, data = <<>>},
  {noreply, NewState};

handle_cast({status, RequestRef, Status}, #state{request_ref = RequestRef} = State) ->
  NewState = State#state{status = Status},
  {noreply, NewState};

handle_cast({data, RequestRef, Data}, #state{request_ref = RequestRef, status = 200, data = Accumulator} = State) ->
  NewState = State#state{data = <<Accumulator/binary, Data/binary>>},
  {noreply, NewState};

handle_cast({data, RequestRef, _Data}, #state{request_ref = RequestRef, status = _Status} = State) ->
  {noreply, State};

handle_cast(
  {fin, RequestRef}, #state{requester_ref = RequesterRef, request_ref = RequestRef, status = 200, data = Data} = State
) ->
  Result = case response_lib:decode_body(Data) of
    {ok, {response, Response}} -> {result, {response, Response}};

    {ok, {error, 10}} -> {retry, {vk_error, 10}};

    {ok, {error, 1}} -> {retry, {vk_error, 1}};

    {ok, {error, ErrorCode}} -> {result, {error, ErrorCode}};

    {error, Reason} -> {retry, {decode_error, Reason}}
  end,

  requester_controller:release(RequesterRef, RequestRef, Result),
  {stop, normal, State};

handle_cast(
  {fin, RequestRef}, #state{requester_ref = RequesterRef, request_ref = RequestRef, status = Status} = State
) ->
  Result = {retry, {http_status_error, Status}},
  requester_controller:release(RequesterRef, RequestRef, Result),
  {stop, normal, State};

handle_cast({error, RequestRef, Reason}, #state{requester_ref = RequesterRef, request_ref = RequestRef} = State) ->
  Result = {retry, Reason},
  requester_controller:release(RequesterRef, RequestRef, Result),
  {stop, normal, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
