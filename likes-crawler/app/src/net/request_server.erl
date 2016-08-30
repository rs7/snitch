-module(request_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, set_status/2, add_data/2, fin/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  status,
  data,
  response
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link(?MODULE, [], []).

set_status(Request, Status) -> gen_server:cast(Request, {status, Status}).

add_data(Request, Data) -> gen_server:cast(Request, {data, Data}).

fin(Request) -> gen_server:cast(Request, fin).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  lager:debug("init"),
  {ok, #state{}}.

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
  lager:debug("response ~p", [Response]),
  NewState = State#state{response = Response},
  {stop, normal, NewState};

handle_cast(fin, #state{status = _Status} = State) ->
  {stop, normal, State};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(Info, State) ->
  lager:warning("Unexpected message ~p", [Info]),
  {noreply, State}.

terminate(Reason, _State) ->
  lager:debug("terminate ~p", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
