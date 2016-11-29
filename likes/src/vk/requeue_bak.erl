-module(requeue_bak).

%%% api
-export([start_link/0, exec/1, get/0, reply/2, retry/2]).

%%%===================================================================
%%% api
%%%===================================================================

start_link() ->
  Pid = spawn_link(fun loop/0),
  register(?MODULE, Pid),
  {ok, Pid}.

exec(Call) ->
  add(self(), Call),
  receive
    {called, Called} -> Called
  end.

get() ->
  ?MODULE ! {get, self()},
  receive
    {got, Got} -> Got
  end.

reply(CallFrom, Called) ->
  metrics:inc(reply),
  CallFrom ! {called, Called}.

retry(CallFrom, Call) ->
  metrics:inc(retry),
  add(CallFrom, Call).

%%%===================================================================
%%% internal
%%%===================================================================

loop() ->
  receive
    {get, From} -> get(From, 100)
  end.

add(CallFrom, Call) -> ?MODULE ! {call, CallFrom, Call}.

get(GetFrom, Count) -> get(GetFrom, Count, []).

get(GetFrom, 0, Got) ->
  GetFrom ! {got, Got},
  loop();

get(GetFrom, Count, Got) ->
  receive
    {call, CallFrom, Call} -> get(GetFrom, Count - 1, [{Call, CallFrom} | Got])
  after 0 -> get(GetFrom, 0, Got)
  end.
