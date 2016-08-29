-module(request_server).

-behaviour(gen_server).

%%% api
-export([start_link/1, response/3, data/3]).

%%% behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% api
%%%===================================================================

start_link(VkRequest) -> gen_server:start_link(?MODULE, VkRequest, []).

response(Request, Status, IsFin) -> gen_server:cast(Request, {response, Status, IsFin}).

data(Request, Data, IsFin) -> gen_server:cast(Request, {data, Data, IsFin}).

%%%===================================================================
%%% behaviour
%%%===================================================================

init(VkRequest) -> {ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
