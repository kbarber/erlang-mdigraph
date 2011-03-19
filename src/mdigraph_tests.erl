%% --------------------------
%% @copyright 2010 Bob.sh
%% @doc mdigraph test routines
%%
%% @end
%% --------------------------

-module(mdigraph_tests).
-include_lib("eunit/include/eunit.hrl").

% This record is copied from the source for testing convenience
%% -record(mdigraph, {vtab = notable :: mnesia:tab(),
%% 		   etab = notable :: mnesia:tab(),
%% 		   ntab = notable :: mnesia:tab(),
%% 		   cyclic = true  :: boolean()}).

new_test_() ->
    {setup, fun() -> mnesia:start() end, fun(ok) -> mnesia:stop() end,  
     [
      ?_assertMatch({mdigraph, _, _, _, true},  mdigraph:new()),
      ?_assertMatch({mdigraph, _, _, _, true},  mdigraph:new([])),
      ?_assertMatch({mdigraph, _, _, _, true},  mdigraph:new([cyclic])),
      ?_assertMatch({mdigraph, _, _, _, false}, mdigraph:new([acyclic])),
      ?_assertMatch({mdigraph, _, _, _, true},  mdigraph:new([protected])),
      ?_assertMatch({mdigraph, _, _, _, true},  mdigraph:new([private])),
      ?_assertMatch({mdigraph, _, _, _, true},  mdigraph:new([cyclic, protected])), 
      ?_assertMatch({mdigraph, _, _, _, true},  mdigraph:new([cyclic, private])), 
      ?_assertMatch({mdigraph, _, _, _, false}, mdigraph:new([acyclic, protected])), 
      ?_assertMatch({mdigraph, _, _, _, false}, mdigraph:new([acyclic, private])),
      ?_assertError(badarg, mdigraph:new([bad_type]))
     ]
    }.


delete_test_() ->
    {setup, fun() -> mnesia:start(), G = mdigraph:new(), G end, fun(_G) -> mnesia:stop() end,  
     fun(G) -> [?_assertMatch(true,  mdigraph:delete(G))] end}.


info_test_() ->
    {setup, fun() -> mnesia:start(), G = mdigraph:new(), G end, fun(_G) -> mnesia:stop() end,  
     fun(G) ->
	     [
	      ?_assertMatch([{cyclicity, cyclic},{memory, _},{protection,protected}],  mdigraph:info(G))
	      %%?_assertMatch([{cyclicity, cyclic},{memory, _},{protection,protected}],  mdigraph:info(G))
	     ]
     end}.


add_delete_vertex_test_() ->
    {setup, fun() -> mnesia:start(), G = mdigraph:new(), G end, fun(_G) -> mnesia:stop() end,  
     fun(G) ->
	     {inorder,
	      [
	       ?_assertMatch(['$v'|0],       mdigraph:add_vertex(G)),
	       ?_assertMatch({['$v'|0],[]},  mdigraph:vertex(G, ['$v'|0])),
	       ?_assertMatch([['$v'|0]],     mdigraph:vertices(G)),
	       ?_assertMatch([['$v'|0]],     mdigraph:source_vertices(G)),
	       ?_assertMatch([['$v'|0]],     mdigraph:sink_vertices(G)),
	       ?_assertMatch(['$v'|1],       mdigraph:add_vertex(G)),
	       ?_assertMatch(['$v'|2],       mdigraph:add_vertex(G)),
	       ?_assertEqual(3,              mdigraph:no_vertices(G)),
	       ?_assertMatch("foo",          mdigraph:add_vertex(G, "foo")),
	       ?_assertMatch({"foo",[]},     mdigraph:vertex(G, "foo")),
	       ?_assertMatch("bar",          mdigraph:add_vertex(G, "bar")),
	       ?_assertMatch({"foo",[]},     mdigraph:vertex(G, "foo")),
	       ?_assertMatch("next",         mdigraph:add_vertex(G, "next", label)),
	       ?_assertEqual(6,              mdigraph:no_vertices(G)),	       
	       ?_assertMatch(true,           mdigraph:del_vertex(G, "next")),
	       ?_assertEqual(5,              mdigraph:no_vertices(G)),
	       ?_assertMatch(true,           mdigraph:del_vertices(G, ["foo", "bar", ['$v'|0], ['$v'|1], ['$v'|2]])),
	       ?_assertEqual(0,              mdigraph:no_vertices(G))
	      ]
	     }
     end}.

vertex_test_() ->
    {setup, 
     fun() -> mnesia:start(),
	      G = mdigraph:new(),
	      V = mdigraph:add_vertex(G),
	      {G, V} end,
     fun(_G) -> mnesia:stop() end,
     fun({G, V}) ->
	     {inorder,
	      [
	       ?_assertMatch([['$v'|0]],     mdigraph:source_vertices(G)),
	       ?_assertMatch([['$v'|0]],     mdigraph:sink_vertices(G)),
	       ?_assertEqual(1,              mdigraph:no_vertices(G)),
	       ?_assertEqual(0,              mdigraph:in_degree(G, V)),
	       ?_assertEqual(0,              mdigraph:out_degree(G, V))
	      ]
	     }
     end}.



add_delete_edge_test_() ->
    {setup, fun() -> mnesia:start(), G = mdigraph:new(), G end, fun(_G) -> mnesia:stop() end,  
     fun(G) ->
	     {inorder,
	      [
	       ?_assertMatch("foo",          mdigraph:add_vertex(G, "foo")),
	       ?_assertMatch("bar",          mdigraph:add_vertex(G, "bar")),
	       ?_assertMatch("next",         mdigraph:add_vertex(G, "next")),
	       ?_assertMatch(['$e'|0],         mdigraph:add_edge(G, "foo", "bar")),
	       ?_assertMatch(['$e'|1],         mdigraph:add_edge(G, "foo", "next")),
	       ?_assertError({bad_vertex, "not_exist"}, mdigraph:add_edge(G, "foo", "not_exist"))
	      ]
	     }
     end}.




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
