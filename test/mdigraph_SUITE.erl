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
    Edges =  [{"A", "B"}, {"A", "C"}, {"B", "D"}, {"C", "D"}, {"D", "E"}, {"E", "F"}],
    Vertices = ["A", "B", "C", "D", "E", "F"],
    [{edges, Edges}, {vertices, Vertices} | Config].

end_per_suite(_Config) ->
    mnesia:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    MG = mdigraph:new(),
    DG = digraph:new(),

    Vertices = ?config(vertices, Config),
    Edges = ?config(edges, Config),
    
    %% init digraph and mdigraph with same data
    %% add vertices
    [mdigraph:add_vertex(MG, V) || V <- Vertices],
    [digraph:add_vertex(DG, V) || V <- Vertices],

    %% add edges
    [mdigraph:add_edge(MG, V1, V2) || {V1, V2} <- Edges],
    [digraph:add_edge(DG, V1, V2) || {V1, V2} <- Edges],

    [{mg, MG}, {dg, DG} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

all() -> 
    [add_vertex].

add_vertex(Config) ->
    MG = ?config(mg, Config),
    DG = ?config(dg, Config),

    %% compare vertices could be in unspecified order
    MG_V = lists:sort(mdigraph:vertices(MG)),
    DG_V = lists:sort(digraph:vertices(DG)),
    ct:log("-> vertices, ~p, ~p ", [MG_V, DG_V]),
    MG_V = DG_V,

    %% compare edges
    MG_E = lists:sort(mdigraph:edges(MG)),
    DG_E = lists:sort(digraph:edges(DG)),
    ct:log("-> edges, ~p, ~p ", [MG_E, DG_E]),
    MG_E = DG_E,
    ok.
