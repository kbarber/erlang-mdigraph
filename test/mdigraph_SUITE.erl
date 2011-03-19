%%%-------------------------------------------------------------------
%%% @author Roman Shestakov <>
%%% @copyright (C) 2011, Roman Shestakov
%%% @doc
%%%
%%% @end
%%% Created : 19 Mar 2011 by Roman Shestakov <>
%%%-------------------------------------------------------------------
-module(mdigraph_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    mnesia:start(),
    Config.

end_per_suite(_Config) ->
    mnesia:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> 
    [add_vertex].

add_vertex(_Config) -> 
    MD = mdigraph:new(),
    DG = digraph:new(),
    VM = mdigraph:add_vertex(MD, "foo"),
    VD = digraph:add_vertex(DG, "foo"),
    VM = VD,
    ok.
