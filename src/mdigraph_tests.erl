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
    stopped = mnesia:stop().

%% @doc vertex
vertex_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V1 = mdigraph:add_vertex(G),
    V2 = mdigraph:vertex(G,V1),
    ?assert({G#digraph.vtab, V1, []} =:= V2),
    stopped = mnesia:stop().

no_verticies_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    mdigraph:no_vertices(G),
    stopped = mnesia:stop().

%% @doc vertices
vertices_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    mdigraph:vertices(G),
    stopped = mnesia:stop().

%% @doc in_degree
in_degree_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V = mdigraph:add_vertex(G),
    mdigraph:in_degree(G,V),
    stopped = mnesia:stop().

%% @doc in_neighbours
in_neighbours_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V = mdigraph:add_vertex(G),
    mdigraph:in_neighbours(G,V),
    stopped = mnesia:stop().

%% @doc in_neighbours
in_edges_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V = mdigraph:add_vertex(G),
    mdigraph:in_edges(G,V),
    stopped = mnesia:stop().

%% @doc out_neighbours
out_degree_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V = mdigraph:add_vertex(G),
    mdigraph:out_degree(G,V),
    stopped = mnesia:stop().


%% @doc out_neighbours
out_neighbours_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V = mdigraph:add_vertex(G),
    mdigraph:out_neighbours(G,V),
    stopped = mnesia:stop().

%% @doc out_edges
out_edges_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V = mdigraph:add_vertex(G),
    mdigraph:out_edges(G,V),
    stopped = mnesia:stop().

no_edges_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    mdigraph:no_edges(G),
    stopped = mnesia:stop().

edges_10_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    mdigraph:edges(G),
    stopped = mnesia:stop().

edges_20_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V1 = mdigraph:add_vertex(G),
    V2 = mdigraph:add_vertex(G),
    E = mdigraph:add_edge(G, V1, V2),
    mdigraph:edges(G, E),
    stopped = mnesia:stop().

edge_20_test()->
    ok = mnesia:start(),
    G = mdigraph:new([cyclic]),
    V1 = mdigraph:add_vertex(G),
    V2 = mdigraph:add_vertex(G),
    E = mdigraph:add_edge(G, V1, V2),
    mdigraph:edge(G, E),
    stopped = mnesia:stop().
