-module(request_server).

-behaviour(gen_server).

%%% api
-export([start_link/2, status/2, data/2, fin/1, error/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  requester_pid,
  request_ref,
  request_data,
  status,
  data
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterPid, RequestRef) -> gen_server:start_link(?MODULE, {RequesterPid, RequestRef}, []).

status(RequestPid, Status) -> gen_server:cast(RequestPid, {status, Status}).

data(RequestPid, Data) -> gen_server:cast(RequestPid, {data, Data}).

fin(RequestPid) -> gen_server:cast(RequestPid, fin).

error(RequestPid, Reason) -> gen_server:cast(RequestPid, {error, Reason}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({RequesterPid, RequestRef}) ->
  {ok, #state{
    requester_pid = RequesterPid,
    request_ref = RequestRef
  }}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({status, 200 = Status}, State) ->
  NewState = State#state{status = Status, data = <<>>},
  {noreply, NewState};

handle_cast({status, Status}, State) ->
  NewState = State#state{status = Status},
  {noreply, NewState};

handle_cast({data, Data}, #state{status = 200, data = Accumulator} = State) ->
  NewState = State#state{data = <<Accumulator/binary, Data/binary>>},
  {noreply, NewState};

handle_cast({data, _Data}, #state{status = _Status} = State) ->
  {noreply, State};

handle_cast(
  fin,
  #state{status = 200, data = Data, requester_pid = RequesterPid, request_ref = RequestRef} = State
) ->

  Result = case response_lib:decode_body(Data) of
    {ok, {response, Response}} -> {result, {response, Response}};

    {ok, {error, 10}} -> {retry, {vk_error, 10}};

    {ok, {error, 1}} -> {retry, {vk_error, 1}};

    {ok, {error, ErrorCode}} -> {result, {error, ErrorCode}};

    {error, Reason} -> {retry, {decode_error, Reason}}
  end,

  requester_server:release(RequesterPid, RequestRef, Result),
  {stop, normal, State};

handle_cast(fin, #state{status = Status, requester_pid = RequesterPid, request_ref = RequestRef} = State) ->
  Result = {retry, {http_status_error, Status}},
  requester_server:release(RequesterPid, RequestRef, Result),
  {stop, normal, State};

handle_cast({error, Reason}, #state{requester_pid = RequesterPid, request_ref = RequestRef} = State) ->
  Result = {retry, {gun_error, Reason}},
  requester_server:release(RequesterPid, RequestRef, Result),
  {stop, normal, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
