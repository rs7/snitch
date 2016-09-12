-module(heap_server).

-behaviour(gen_server).

%%% api
-export([start_link/0, call/1, reserve/0, release/2]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(INFO_STAT_TIMEOUT, 10 * 1000).
-define(SET_WORKERS_COUNT_TIMEOUT, 1 * 1000).
-define(RESERVE_SIZE, 1000).

-record(stat, {
  call_count = 0,
  reply_count = 0,
  reserve_count = 0,
  release_count = 0,
  retry_count = 0
}).

-record(state, {
  heap = [],
  reserved = #{},
  workers_count = 0,
  stat = #stat{}
}).

%%%===================================================================
%%% api
%%%===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call(RequestData) -> gen_server:call(?MODULE, {call, RequestData}, infinity).

reserve() -> gen_server:call(?MODULE, reserve, infinity).

release(RequestRef, Result) -> gen_server:cast(?MODULE, {release, RequestRef, Result}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init([]) ->
  self() ! info_stat,
  self() ! set_workers_count,
  {ok, #state{}}.

%%%===================================================================
%%% call
%%%===================================================================

handle_call({call, RequestData}, RequestFrom, #state{heap = Heap, stat = Stat} = State) ->
  NewHeap = Heap ++ [{RequestData, RequestFrom}],
  NewState = State#state{
    heap = NewHeap,
    stat = Stat#stat{call_count = Stat#stat.call_count + 1}
  },
  {noreply, NewState};

%%%===================================================================
%%% reserve
%%%===================================================================

%%TODO: монитор на резервирующий процесс
%%TODO: ожидание, пока не наберётся Count?
handle_call(
  reserve, {ReservePid, _Tag},
  #state{heap = Heap, reserved = Reserved, stat = Stat} = State
) ->

  MonitorRef = monitor(process, ReservePid),

  {ReservedHeap, NewHeap} = util:list_split(Heap, ?RESERVE_SIZE),

  Helper = [{make_ref(), HeapItem} || HeapItem <- ReservedHeap],

  Result = [{RequestRef, RequestData} || {RequestRef, {RequestData, _RequestFrom}} <- Helper],

  ReservedMap = maps:from_list(Helper),

  NewReserved = maps:merge(Reserved, ReservedMap),

  NewState = State#state{
    heap = NewHeap,
    reserved = NewReserved,
    stat = Stat#stat{reserve_count = Stat#stat.reserve_count + length(ReservedHeap)}
  },
  {reply, {ok, Result}, NewState};

%%%===================================================================

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% result
%%%===================================================================

handle_cast({release, RequestRef, {result, Result}}, #state{reserved = Reserved, stat = Stat} = State) ->
  {HeapItem, NewReserved} = maps:take(RequestRef, Reserved),

  {_RequestData, RequestFrom} = HeapItem,

  gen_server:reply(RequestFrom, Result),

  NewState = State#state{
    reserved = NewReserved,
    stat = Stat#stat{
      reply_count = Stat#stat.reply_count + 1,
      release_count = Stat#stat.release_count + 1
    }
  },
  {noreply, NewState};

%%%===================================================================
%%% retry
%%%===================================================================

%% TODO: нужна ли здесь причина повтора (Reason)?

handle_cast({release, RequestRef, {retry, Reason}}, #state{heap = Heap, reserved = Reserved, stat = Stat} = State) ->
  {HeapItem, NewReserved} = maps:take(RequestRef, Reserved),

  lager:warning("retry ~p ~p", [HeapItem, Reason]),

  NewHeap = [HeapItem | Heap],
  NewState = State#state{
    heap = NewHeap,
    reserved = NewReserved,
    stat = Stat#stat{
      retry_count = Stat#stat.retry_count + 1,
      release_count = Stat#stat.release_count + 1
    }
  },
  {noreply, NewState};

%%%===================================================================

handle_cast(_Request, State) -> {noreply, State}.

handle_info(
  info_stat,
  #state{
    heap = Heap,
    workers_count = WorkersCount,
    stat = #stat{
      call_count = CallCount,
      reply_count = ReplyCount,
      reserve_count = ReserveCount,
      release_count = ReleaseCount,
      retry_count = RetryCount
    }
  } = State
) ->
  erlang:send_after(?INFO_STAT_TIMEOUT, self(), info_stat),

  {message_queue_len, MessageQueueLen} = erlang:process_info(self(), message_queue_len),
  lager:debug(
    "heap ~B workers ~B mailbox ~B "
    "call ~B reply ~B wait ~B "
    "reserve ~B release ~B reserved ~B "
    "retry ~B ",
    [
      length(Heap), WorkersCount, MessageQueueLen,
      CallCount, ReplyCount, CallCount - ReplyCount,
      ReserveCount, ReleaseCount, ReserveCount - ReleaseCount,
      RetryCount
    ]
  ),
  {noreply, State};

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
  lager:warning("reserve process DOWN ~p ~p", [Pid, Reason]),
  {noreply, State};

handle_info(set_workers_count, #state{heap = Heap, workers_count = WorkersCount} = State) ->
  erlang:send_after(?SET_WORKERS_COUNT_TIMEOUT, self(), set_workers_count),
  NewWorkersCount = util:ceil(length(Heap) / 1000),
  if
    NewWorkersCount =/= WorkersCount -> workers_supervisor:set_workers_count(NewWorkersCount);
    true -> ok
  end,
  NewState = State#state{workers_count = NewWorkersCount},
  {noreply, NewState};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
