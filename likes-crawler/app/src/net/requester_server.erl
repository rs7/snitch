-module(requester_server).

-behaviour(gen_server).

%%% api
-export([start_link/1, get_request_data/1, gproc_key/1]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(WorkerId) -> gen_server:start_link(?MODULE, WorkerId, []).

get_request_data(RequesterPid) -> gen_server:call(RequesterPid, get_request_data).

gproc_key(WorkerId) -> {n, l, {requester, WorkerId}}.

%%%===================================================================
%%% behaviour
%%%===================================================================

init(WorkerId) ->
  gproc:reg(gproc_key(WorkerId)),
  {ok, #state{}}.

handle_call(get_request_data, _From, State) ->
  Reply = mock:get_request_data(),
  {reply, Reply, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%repeatable_request(Connection, Request, RetryNumber) ->
%%  lager:debug("~B/~p", [RetryNumber, Request]),
%%  case run(Connection, Request) of
%%    {result, Result} -> Result;
%%    {retry, Reason} ->
%%      error_warning(Reason, Request, RetryNumber),
%%      timer:sleep(retry_timeout(RetryNumber)),
%%      repeatable_request(Connection, Request, RetryNumber + 1)
%%  end.
%%
%%request(Connection, Request) ->
%%  Result = shotgun_request(Connection, Request),
%%  case Result of
%%    {ok, #{status_code := 200, body := Body}} -> parse_body(Body);
%%    {ok, #{status_code := StatusCode}} -> {retry, {status_code, StatusCode}};
%%    {error, Reason} -> {retry, {shotgun, Reason}}
%%  end.
%%
%%
%%
%%vk_error(1) -> {retry, {vk_error, 1}};
%%vk_error(10) -> {retry, {vk_error, 10}};
%%vk_error(Code) -> {result, {error, Code}}.
%%
%%retry_timeout(1) -> 10 * 1000;
%%retry_timeout(2) -> rand_seconds(30, 60);
%%retry_timeout(3) -> rand_seconds(60, 120);
%%retry_timeout(5) -> rand_seconds(120, 300).
%%
%%error_warning(_Reason, _Request, TryNumber) when TryNumber < 5 -> ok;
%%error_warning(Reason, Request, TryNumber) -> lager:warning(
%%  "Request error ~B times~n~p~n~p", [TryNumber, Request, Reason]
%%).

%%%===================================================================
%%% util
%%%===================================================================

%%rand_seconds(From, To) -> (From + rand:uniform(To - From)) * 1000.
