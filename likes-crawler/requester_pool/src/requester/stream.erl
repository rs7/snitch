-module(stream).

-behaviour(gen_server).

%%% api
-export([start_link/2, status/2, data/2, fin/1, error/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(NAME(StreamRef), {via, identifiable, {?MODULE, StreamRef}}).

-record(state, {stream_ref, requester_ref, status, data}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(StreamRef, RequesterRef) -> gen_server:start_link(?NAME(StreamRef), ?MODULE, {StreamRef, RequesterRef}, []).

status(StreamRef, Status) -> gen_server:cast(?NAME(StreamRef), {status, Status}).

data(StreamRef, Data) -> gen_server:cast(?NAME(StreamRef), {data, Data}).

fin(StreamRef) -> gen_server:cast(?NAME(StreamRef), fin).

error(StreamRef, Reason) -> gen_server:cast(?NAME(StreamRef), {error, Reason}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({StreamRef, RequesterRef}) ->
  NewState = #state{stream_ref = StreamRef, requester_ref = RequesterRef},
  {ok, NewState}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({status, 200 = Status}, State) ->
  NewState = State#state{status = Status, data = <<>>},
  {noreply, NewState};

handle_cast({status, Status}, State) ->
  NewState = State#state{status = Status},
  send_error({http_status_error, Status}, NewState),
  {noreply, NewState};

handle_cast({data, Data}, #state{status = 200, data = Accumulator} = State) ->
  NewState = State#state{data = <<Accumulator/binary, Data/binary>>},
  {noreply, NewState};

handle_cast({data, _Data}, #state{status = _Status} = State) -> {noreply, State};

handle_cast(fin, #state{status = 200} = State) ->
  send_decode_result(State),
  {stop, normal, State};

handle_cast(fin, #state{status = Status} = State) ->
  send_error({http_status_error, Status}, State),
  {stop, normal, State};

handle_cast({error, Reason}, State) ->
  send_error(Reason, State),
  {stop, normal, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

send_error(Reason, #state{stream_ref = StreamRef, requester_ref = RequesterRef}) ->
  lager:info("error ~p", [Reason]).

send_result(Result, #state{stream_ref = StreamRef, requester_ref = RequesterRef}) ->
  lager:info("result ~p", [Result]).

send_decode_result(#state{data = Data} = State) ->
  case response_lib:decode_body(Data) of
    {ok, {response, Response}} -> send_result({response, Response}, State);
    {ok, {error, 10}} -> send_error({vk_error, 10}, State);
    {ok, {error, 1}} -> send_error({vk_error, 1}, State);
    {ok, {error, ErrorCode}} -> send_result({error, ErrorCode}, State);
    {error, Reason} -> send_error({decode_error, Reason}, State)
  end.
