-module(hidden_friends).

%%% api
-export([process/2]).

%%%===================================================================
%%% api
%%%===================================================================

process(User, CallFunc) ->
  case apply(CallFunc, [{'friends.get', #{user_id => User}}]) of

    {response, [1 | _]} -> lager:info("Find! ~B", [User]), next;

    {response, [_ | _]} -> next;

    {response, []} -> next;

    {error, 15} -> next;

    Result -> lager:warning("Unexpected result ~p~n~p", [User, Result]), next

  end.
