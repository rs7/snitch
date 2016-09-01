-module(request_server).

-behaviour(gen_server).

%%% api
-export([start_link/3, status/2, data/2, fin/1, error/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  requester_pid,
  connection_pid,
  status,
  data,
  request_data,
  retry_count
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterPid, ConnectionPid, RequestData) ->
  gen_server:start_link(?MODULE, [RequesterPid, ConnectionPid, RequestData], []).

status(RequestPid, Status) -> gen_server:cast(RequestPid, {status, Status}).

data(RequestPid, Data) -> gen_server:cast(RequestPid, {data, Data}).

fin(RequestPid) -> gen_server:cast(RequestPid, fin).

error(RequestPid) -> gen_server:cast(RequestPid, error).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([RequesterPid, ConnectionPid, RequestData]) ->
  self() ! start,
  {ok, #state{
    requester_pid = RequesterPid,
    connection_pid = ConnectionPid,
    request_data = RequestData,
    retry_count = 0
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

handle_cast(fin, #state{status = 200, data = Data, requester_pid = RequesterPid} = State) ->
  Next = case response_lib:decode_body(Data) of
    {ok, {response, Response}} -> {result, {response, Response}};

    {ok, {error, 10}} -> retry;

    {ok, {error, 1}} -> retry;

    {ok, {error, ErrorCode}} -> {result, {error, ErrorCode}};

    {error, _Reason} -> retry
  end,

  case Next of
    {result, Result} ->
      requester_server:result(RequesterPid, Result),
      {stop, normal, State};

    retry ->
      self() ! start,
      {noreply, State}
  end;

handle_cast(fin, #state{status = _Status} = State) ->
  self() ! start,
  {noreply, State};

handle_cast(error, State) ->
  self() ! start,
  {noreply, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  start,
  #state{connection_pid = ConnectionPid, request_data = RequestData, retry_count = RetryCount} = State
) ->
  connection_server:register_request(ConnectionPid, RequestData, self()),

  NewState = State#state{
    status = undefined,
    data = undefined,
    retry_count = RetryCount + 1
  },

  {noreply, NewState};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
