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

new_test_() ->
    {setup, fun() -> mnesia:start() end, fun(ok) -> mnesia:stop() end,  
     [
      ?_assertMatch({mdigraph, _, _, _, tru},  mdigraph:new())
     ]
    }.



%% %% @doc new/0
%% new_10_test()->
%%     ok = mnesia:start(),
%%     mdigraph:new(),
%%     stopped = mnesia:stop().

%% %% @doc new/1
%% new_20_test()-> 
%%     ok = mnesia:start(),
%%     mdigraph:new([cyclic]),
%%     stopped = mnesia:stop().
    
%% %% @doc new/2
%% new_30_test()->
%%     ok = mnesia:start(),
%%     {digraph,'vertices-foobar','edges-foobar','neighbours-foobar',true} = 
%%         mdigraph:new("foobar", [cyclic]),
%%     stopped = mnesia:stop().

%% %% @doc Try deleting an mdigraph
%% delete_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     {atomic, ok} = mdigraph:delete(G),
%%     stopped = mnesia:stop().

%% %% @doc Digraph info
%% info_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     [_|_] = mdigraph:info(G),
%%     stopped = mnesia:stop().  

%% %% @doc add_vertex/1
%% add_vertex_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     ['$v'|0] = mdigraph:add_vertex(G),
%%     stopped = mnesia:stop().

%% %% @doc add_vertex/2
%% add_vertex_20_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     "foo" = mdigraph:add_vertex(G, "foo"),
%%     stopped = mnesia:stop().

%% %% @doc del_vertex/2
%% del_vertex_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     V = mdigraph:add_vertex(G),
%%     true = mdigraph:del_vertex(G,V),
%%     stopped = mnesia:stop().

%% %% @doc del_vertices/2
%% del_vertices_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     V = mdigraph:add_vertex(G),
%%     true = mdigraph:del_vertices(G,[V]),
%%     0 = mdigraph:no_vertices(G),
%%     stopped = mnesia:stop().

%% %% @doc vertex/2
%% vertex_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     V = mdigraph:add_vertex(G),
%%     VT = G#digraph.vtab,
%%     {VT, V, []} = mdigraph:vertex(G,V),
%%     stopped = mnesia:stop().

%% %% @doc no_verticies/1
%% no_verticies_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     mdigraph:add_vertex(G),
%%     1 = mdigraph:no_vertices(G),
%%     stopped = mnesia:stop().

%% %% @doc vertices/1
%% vertices_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new([cyclic]),
%%     mdigraph:add_vertex(G),
%%     [['$v'|0]] = mdigraph:vertices(G),
%%     stopped = mnesia:stop().

%% %% @doc source_vertices/1
%% %% TODO: add real test, this is a placeholder
%% source_vertices_10_test() ->
%%     ok.

%% %% @doc sink_vertices/1
%% %% TODO: add real test, this is a placeholder
%% sink_vertices_10_test() ->
%%     ok.

%% %% @doc add_edge/3
%% add_edge_10_test() ->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     V1 = mdigraph:add_vertex(G),
%%     V2 = mdigraph:add_vertex(G),
%%     ['$e'|0] = mdigraph:add_edge(G,V1,V2),
%%     stopped = mnesia:stop().    

%% %% @doc add_edge/4
%% add_edge_20_test() ->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     V1 = mdigraph:add_vertex(G),
%%     V2 = mdigraph:add_vertex(G),
%%     ['$e'|0] = mdigraph:add_edge(G,V1,V2,"foo"),
%%     stopped = mnesia:stop().

%% %% @doc del_edge/2
%% %% TODO: check response of call
%% del_edge_10_test() ->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     V1 = mdigraph:add_vertex(G),
%%     V2 = mdigraph:add_vertex(G),
%%     E = mdigraph:add_edge(G,V1,V2),
%%     A = mdigraph:del_edge(G,E),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().    

%% %% @doc del_edge/2
%% %% TODO: check response of call
%% del_edges_10_test() ->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     V1 = mdigraph:add_vertex(G),
%%     V2 = mdigraph:add_vertex(G),
%%     E = mdigraph:add_edge(G,V1,V2),
%%     A = mdigraph:del_edges(G,[E]),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().    

%% %% @doc del_path/2
%% %% TODO: check response of call
%% del_path_10_test() ->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     V1 = mdigraph:add_vertex(G),
%%     V2 = mdigraph:add_vertex(G),
%%     V3 = mdigraph:add_vertex(G),
%%     mdigraph:add_edge(G,V1,V2),
%%     mdigraph:add_edge(G,V2,V3),
%%     A = mdigraph:del_path(G,V1,V3),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().    


%% %% @doc in_degree
%% %% TODO: check response of call
%% in_degree_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     V = mdigraph:add_vertex(G),
%%     mdigraph:in_degree(G,V),
%%     stopped = mnesia:stop().

%% %% @doc in_neighbours
%% %% TODO: check response of call
%% in_neighbours_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new([cyclic]),
%%     V = mdigraph:add_vertex(G),
%%     mdigraph:in_neighbours(G,V),
%%     stopped = mnesia:stop().

%% %% @doc in_neighbours
%% %% TODO: check response of call
%% in_edges_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new([cyclic]),
%%     V = mdigraph:add_vertex(G),
%%     mdigraph:in_edges(G,V),
%%     stopped = mnesia:stop().

%% %% @doc out_neighbours
%% %% TODO: check response of call
%% out_degree_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new([cyclic]),
%%     V = mdigraph:add_vertex(G),
%%     mdigraph:out_degree(G,V),
%%     stopped = mnesia:stop().


%% %% @doc out_neighbours
%% %% TODO: check response of call
%% out_neighbours_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new([cyclic]),
%%     V = mdigraph:add_vertex(G),
%%     mdigraph:out_neighbours(G,V),
%%     stopped = mnesia:stop().

%% %% @doc out_edges/2
%% %% TODO: check response of call
%% out_edges_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new([cyclic]),
%%     V = mdigraph:add_vertex(G),
%%     mdigraph:out_edges(G,V),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% no_edges_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new([cyclic]),
%%     mdigraph:no_edges(G),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% edges_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new(),
%%     V1 = mdigraph:add_vertex(G),
%%     V2 = mdigraph:add_vertex(G),
%%     mdigraph:add_edge(G, V1, V2),
%%     A = mdigraph:edges(G),
%%     ?debugFmt("A:~p~n", [A]),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% edges_20_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new([cyclic]),
%%     V1 = mdigraph:add_vertex(G),
%%     V2 = mdigraph:add_vertex(G),
%%     E = mdigraph:add_edge(G, V1, V2),
%%     mdigraph:edges(G, E),
%%     stopped = mnesia:stop().

%% %% TODO: check response of call
%% edge_10_test()->
%%     ok = mnesia:start(),
%%     G = mdigraph:new([cyclic]),
%%     V1 = mdigraph:add_vertex(G),
%%     V2 = mdigraph:add_vertex(G),
%%     E = mdigraph:add_edge(G, V1, V2),
%%     mdigraph:edge(G, E),
%%     stopped = mnesia:stop().
