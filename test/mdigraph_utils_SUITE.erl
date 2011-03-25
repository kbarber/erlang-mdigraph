%%%-------------------------------------------------------------------
%%% @author Roman Shestakov <>
%%% @copyright (C) 2011, Roman Shestakov
%%% @doc
%%%
%%% @end
%%% Created : 19 Mar 2011 by Roman Shestakov <>
%%%-------------------------------------------------------------------
-module(mdigraph_utils_SUITE).

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

end_per_testcase(_TestCase, Config) ->
    MG = ?config(mg, Config),
    DG = ?config(dg, Config),
    %% delete graphs
    mdigraph:delete(MG),
    digraph:delete(DG),
    ok.

all() -> 
    [
     utils
    ].


utils(Config) ->
    MG = ?config(mg, Config),
    DG = ?config(dg, Config),
    Vertices = ?config(vertices, Config),
    %% topsort
    MG_T = mdigraph_utils:topsort(MG),
    DG_T = digraph_utils:topsort(DG),
    MG_T = DG_T,
    ct:log("-> topsort, ~p, ~p ", [MG_T, DG_T]),
    %% arborescence_root
    MG_R = mdigraph_utils:arborescence_root(MG),
    DG_R = digraph_utils:arborescence_root(DG),
    MG_R = DG_R,
    ct:log("-> arborescence_root, ~p, ~p ", [MG_R, DG_R]),
    %% components
    MG_C = mdigraph_utils:components(MG),
    DG_C = digraph_utils:components(DG),
    MG_C = DG_C,
    ct:log("-> components, ~p, ~p ", [MG_C, DG_C]),
    %% condensation
    MG2 = mdigraph_utils:condensation(MG),
    DG2 = digraph_utils:condensation(DG),
    %% compare vertices
    MG_V = lists:sort(mdigraph:vertices(MG2)),
    DG_V = lists:sort(digraph:vertices(DG2)),
    ct:log("-> vertices, ~p, ~p ", [MG2, DG2]),
    MG_V = DG_V,
    %% compare edges
    MG_E = lists:sort(mdigraph:edges(MG2)),
    DG_E = lists:sort(digraph:edges(DG2)),
    ct:log("-> edges, ~p, ~p ", [MG_E, DG_E]),
    MG_E = DG_E,
    %% cyclic_stong_components
    MG_SC = mdigraph_utils:cyclic_strong_components(MG),
    DG_SC = digraph_utils:cyclic_strong_components(DG),
    MG_SC = DG_SC,
    ct:log("-> cyclic strong components, ~p, ~p ", [MG_SC, DG_SC]),
    %% is acyclic
    MG_A = mdigraph_utils:is_acyclic(MG),
    DG_A = digraph_utils:is_acyclic(DG),
    MG_A = DG_A,
    ct:log("-> is acyclic ~p, ~p ", [MG_A, DG_A]),
    %% is_arborescence
    MG_AB = mdigraph_utils:is_arborescence(MG),
    DG_AB = digraph_utils:is_arborescence(DG),
    MG_AB = DG_AB,
    ct:log("-> is arborescence ~p, ~p ", [MG_AB, DG_AB]),
    %% is_tree
    MG_TR = mdigraph_utils:is_tree(MG),
    DG_TR = digraph_utils:is_tree(DG),
    MG_TR = DG_TR,
    ct:log("-> is tree ~p, ~p ", [MG_TR, DG_TR]),
    %% postorder
    MG_PO = mdigraph_utils:postorder(MG),
    DG_PO = digraph_utils:postorder(DG),
    MG_PO = DG_PO,
    ct:log("-> post order ~p, ~p ", [MG_PO, DG_PO]),
    %% preorder
    MG_PRO = mdigraph_utils:preorder(MG),
    DG_PRO = digraph_utils:preorder(DG),
    MG_PRO = DG_PRO,
    ct:log("-> pre order ~p, ~p ", [MG_PRO, DG_PRO]),
    %% reachable
    MG_RBL = mdigraph_utils:reachable(Vertices, MG),
    DG_RBL = digraph_utils:reachable(Vertices, DG),
    MG_RBL = DG_RBL,
    ct:log("-> reachable ~p, ~p ", [MG_RBL, DG_RBL]),
    %% reachable neighbours
    MG_RBL_N = mdigraph_utils:reachable_neighbours(Vertices, MG),
    DG_RBL_N = digraph_utils:reachable_neighbours(Vertices, DG),
    MG_RBL_N = DG_RBL_N,
    ct:log("-> reachable neighbours ~p, ~p ", [MG_RBL_N, DG_RBL_N]),
    %% strong components
    MG_STR = mdigraph_utils:strong_components(MG),
    DG_STR = digraph_utils:strong_components(DG),
    MG_STR = DG_STR,
    ct:log("-> strong components ~p, ~p ", [MG_STR, DG_STR]),
    %% subgraph
    MG3 = mdigraph_utils:subgraph(MG, ["B", "C", "D", "E"]),
    DG3 = digraph_utils:subgraph(DG, ["B", "C", "D", "E"]),
    %% compare vertices
    MG_V3 = lists:sort(mdigraph:vertices(MG3)),
    DG_V3 = lists:sort(digraph:vertices(DG3)),
    ct:log("-> vertices, ~p, ~p ", [MG_V3, DG_V3]),
    MG_V3 = DG_V3,
    %% compare edges
    MG_E3 = lists:sort(mdigraph:edges(MG3)),
    DG_E3 = lists:sort(digraph:edges(DG3)),
    ct:log("-> edges, ~p, ~p ", [MG_E3, DG_E3]),
    MG_E3 = DG_E3,
    ok.

