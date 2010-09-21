%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc Primary entry point for tests.
%%
%% @end
%% --------------------------

-module(mdigraph_runtests).
-export([start/0]).

start() ->
    error_logger:tty(false),
    eunit:test(mdigraph,[verbose]),
    halt().
