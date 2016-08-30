-module(requests_buffer).

%%% api
-export([get_requests/1]).

%%% internal
-export([request_process/0]).

%%%===================================================================
%%% api
%%%===================================================================

get_requests(0) -> [];
get_requests(Count) ->
  Request = mock_request(),
  {ok, Pid} = request_server:start_link(),
  [{Request, Pid} | get_requests(Count - 1)].

%%%===================================================================
%%% internal
%%%===================================================================

mock_request() -> {'utils.getServerTime', #{}}.

request_process() ->
  receive
    Message ->
      lager:debug("RM ~p ~p", [self(), Message]),
      request_process()
  end.
