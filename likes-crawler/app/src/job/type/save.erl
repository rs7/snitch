-module(save).

-behaviour(gen_job).

%%% behavior
-export([process/2]).

%%%===================================================================
%%% behavior
%%%===================================================================

process(_Priority, {Liker, OwnerId, PhotoId}) ->
  {ok, OutputFilename} = application:get_env(output_file),
  WriteFileResult = file:write_file(OutputFilename, [output_line({Liker, OwnerId, PhotoId}), $\n], [append]),
  lager:info("id~B photo~B_~B ~p", [Liker, OwnerId, PhotoId, WriteFileResult]),
  {ok, []}.

output_line({Liker, OwnerId, PhotoId}) ->
  io_lib:format(
    "<a href=\"http://vk.com/id~B\">id~B</a> "
    "<a href=\"http://vk.com/id~B\">id~B</a> "
    "<a href=\"http://vk.com/photo~B_~B\">photo~B_~B</a> "
    "<br>",
    [Liker, Liker, OwnerId, OwnerId, OwnerId, PhotoId, OwnerId, PhotoId]
  ).
