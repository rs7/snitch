-module(request_server).

-behaviour(gen_server).

%%% api
-export([start_link/2, set_status/2, add_data/2, fin/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  requester_pid,
  connection_pid,
  status,
  data,
  response
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequesterPid, ConnectionPid) ->
  gen_server:start_link(?MODULE, [RequesterPid, ConnectionPid], []).

set_status(RequestPid, Status) -> gen_server:cast(RequestPid, {status, Status}).

add_data(RequestPid, Data) -> gen_server:cast(RequestPid, {data, Data}).

fin(RequestPid) -> gen_server:cast(RequestPid, fin).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([RequesterPid, ConnectionPid]) ->
  self() ! start,
  {ok, #state{
    requester_pid = RequesterPid,
    connection_pid = ConnectionPid
  }}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({status, 200 = Status}, State) ->
  NewState = State#state{status = Status, data = <<>>},
  {noreply, NewState};

handle_cast({status, Status}, State) ->
  NewState = State#state{status = Status},
  {noreply, NewState};

handle_cast({data, Data}, #state{status = 200, data = Acc} = State) ->
  NewState = State#state{data = <<Acc/binary, Data/binary>>},
  {noreply, NewState};

handle_cast({data, _Data}, #state{status = _Status} = State) ->
  {noreply, State};

handle_cast(fin, #state{status = 200, data = Data} = State) ->
  Response = response_lib:decode_body(Data),
  %lager:debug("response ~p", [Response]),
  NewState = State#state{response = Response},
  {stop, normal, NewState};

handle_cast(fin, #state{status = _Status} = State) ->
  {stop, normal, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(start,
  #state{requester_pid = RequesterPid, connection_pid = ConnectionPid} = State
) ->
  RequestData = requester_server:get_request_data(RequesterPid),
  connection_server:register_request(ConnectionPid, RequestData, self()),
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
