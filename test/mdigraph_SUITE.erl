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
    MG = mdigraph:new(),
    DG = digraph:new(),
    [{mg, MG}, {dg, DG} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> 
    [add_vertex].

add_vertex(Config) ->
    MG = ?config(mg, Config),
    DG = ?config(dg, Config),
 
    V1 = mdigraph:add_vertex(MG, "foo"),
    V2 = digraph:add_vertex(DG, "foo"),
    V1 = V2,
 
    

   ok.
