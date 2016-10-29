-module(request).

-behaviour(gen_server).

%%% api
-export([start_link/2, status/2, data/2, fin/1, error/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER_NAME(RequestRef), {via, identifiable, {?MODULE, RequestRef}}).

-record(state, {
  request_ref,
  requester_ref,
  status,
  data
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(RequestRef, RequesterRef) ->
  gen_server:start_link(?SERVER_NAME(RequestRef), ?MODULE, {RequestRef, RequesterRef}, []).

status(RequestRef, Status) -> gen_server:cast(?SERVER_NAME(RequestRef), {status, Status}).

data(RequestRef, Data) -> gen_server:cast(?SERVER_NAME(RequestRef), {data, Data}).

fin(RequestRef) -> gen_server:cast(?SERVER_NAME(RequestRef), fin).

error(RequestRef, Reason) -> gen_server:cast(?SERVER_NAME(RequestRef), {error, Reason}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init({RequestRef, RequesterRef}) ->
  NewState = #state{
    request_ref = RequestRef,
    requester_ref = RequesterRef
  },
  {ok, NewState}.

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

handle_cast({data, _Data}, #state{status = _Status} = State) -> {noreply, State};

handle_cast(fin, #state{request_ref = RequestRef, requester_ref = RequesterRef, status = 200, data = Data} = State) ->
  case response_lib:decode_body(Data) of
    {ok, {response, Response}} -> requester_controller:result(RequesterRef, RequestRef, {response, Response});

    {ok, {error, 10}} -> requester_controller:retry(RequesterRef, RequestRef, {vk_error, 10});

    {ok, {error, 1}} -> requester_controller:retry(RequesterRef, RequestRef, {vk_error, 1});

    {ok, {error, ErrorCode}} -> requester_controller:result(RequesterRef, RequestRef, {error, ErrorCode});

    {error, Reason} -> requester_controller:retry(RequesterRef, RequestRef, {decode_error, Reason})
  end,
  {stop, normal, State};

handle_cast(fin, #state{request_ref = RequestRef, requester_ref = RequesterRef, status = Status} = State) ->
  requester_controller:retry(RequesterRef, RequestRef, {http_status_error, Status}),
  {stop, normal, State};

handle_cast({error, Reason}, #state{request_ref = RequestRef, requester_ref = RequesterRef} = State) ->
  requester_controller:retry(RequesterRef, RequestRef, Reason),
  {stop, normal, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
