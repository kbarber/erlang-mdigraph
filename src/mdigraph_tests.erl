%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc mdigraph test routines
%%
%% @end
%% --------------------------

-module(mdigraph_tests).
-include_lib("eunit/include/eunit.hrl").

% This record is copied from the source for testing convenience
-record(digraph, {vtab = notable :: ets:tab(),
		  etab = notable :: ets:tab(),
		  ntab = notable :: ets:tab(),
	          cyclic = true  :: boolean()}).

%% @doc Try a new() to see if it crashes
new_10_test()-> 
    ok = mnesia:start(),
    mdigraph:new([cyclic]),
    stopped = mnesia:stop().
    
%% @doc Test with a table prefix
new_20_test()->
    ok = mnesia:start(),
    mdigraph:new("foobar", [cyclic]),
    stopped = mnesia:stop().

%% @doc Try deleting an mdigraph
delete_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    mdigraph:delete(G),
    stopped = mnesia:stop().

%% @doc Digraph info
info_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    mdigraph:info(G),
    stopped = mnesia:stop().  

add_vertex_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    mdigraph:add_vertex(G),
    ?debugFmt("~nTables: ~p~nVTAB: ~p~nKeys: ~p~n", 
        [ mnesia:system_info(tables), G#digraph.vtab, mnesia:transaction(fun() -> mnesia:all_keys(G#digraph.vtab) end) ]
    ),
    stopped = mnesia:stop().

%% @doc vertex
vertex_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V = mdigraph:add_vertex(G),
    V2 = mdigraph:vertex(G,V),
    
    ?debugFmt("~nV: ~p~nV2: ~p~nKeys: ~p~n", 
        [ V,V2,mnesia:transaction(fun() -> mnesia:all_keys(G#digraph.vtab) end) ]
    ),
    ?assert(V =:= V2).

no_verticies_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    mdigraph:no_vertices(G).

%% @doc vertices
vertices_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    mdigraph:vertices(G).

%% @doc in_degree
in_degree_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V = "foo", %TODO
    mdigraph:in_degree(G,V).

%% @doc in_neighbours
in_neighbours_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V = "foo", %TODO
    mdigraph:in_neighbours(G,V).

%% @doc in_neighbours
in_edges_10_test()->
    G = mdigraph:new([cyclic]),
    V = "foo", %TODO
    mdigraph:in_edges(G,V).

%% @doc out_neighbours
out_degree_10_test()->
    G = mdigraph:new([cyclic]),
    V = "foo", %TODO
    mdigraph:out_degree(G,V).


%% @doc out_neighbours
out_neighbours_10_test()->
    G = mdigraph:new([cyclic]),
    V = "foo", %TODO
    mdigraph:out_neighbours(G,V).

%% @doc out_edges
out_edges_10_test()->
    G = mdigraph:new([cyclic]),
    V = "foo", %TODO
    mdigraph:out_edges(G,V).

no_edges_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    mdigraph:no_edges(G).

edges_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    mdigraph:edges(G).

edges_20_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V = "foo",
    5 = mdigraph:edges(G, V).

edge_20_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V = "foo",
    V = mdigraph:edge(G, V).
