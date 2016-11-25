-module(work_queue).

-behaviour(gen_server).

%%% api
-export([start_link/0]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(REPLY_MESSAGE(RequestRef, Reply), {reply, RequestRef, Reply}).

-record(state, {free, reserve}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Request, Priority, ReplyTo) -> gen_server:call(?MODULE, {add, Request, Priority, ReplyTo}).

wait_reply(RequestRef, Timeout) ->
  receive
    ?REPLY_MESSAGE(RequestRef, Reply) -> {ok, Reply}
  after Timeout -> {error, timeout}
  end.

reply({RequestRef, ReplyTo}, Reply) -> ReplyTo ! ?REPLY_MESSAGE(RequestRef, Reply).

take(Count) -> gen_server:call(?MODULE, {take, Count}).

ack(RequestRef) -> gen_server:cast(?MODULE, {ack, RequestRef}).

nack(RequestRef) -> gen_server:cast(?MODULE, {nack, RequestRef}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  NewState = #state{free = gb_sets:new(), reserve = dict:new()},
  {ok, NewState}.

handle_call({add, Request, Priority}, _From, #state{free = Free} = State) ->
  RequestRef = make_ref(),

  NewFree = gb_sets:add({Priority, RequestRef, Request}, Free),

  NewState = State#state{free = NewFree},
  {reply, {ok, RequestRef}, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({nack, RequestRef}, #state{free = Free, reserve = Reserve} = State) ->

  {ok, {Priority, RequestRef, Request}} = dict:find(RequestRef, Reserve),

  NewReserve = dict:erase(RequestRef, Free),
  NewFree = gb_sets:add({Priority, RequestRef, Request}, Free),

  NewState = State#state{
    free = NewFree,
    reserve = NewReserve
  },
  {noreply, NewState};

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
