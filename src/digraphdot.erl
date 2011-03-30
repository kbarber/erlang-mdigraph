-module(digraphdot).
-compile([export_all]).

-define( QUOTED(Val), $",Val,$"). 

%%-type vertices() :: [mdigraph:vertex()].
-type mdigraph() :: [mdigraph:mdigraph()].

-record(graph_attributes, {ratio = auto, ranksep = 0.75}).

init_graph() ->
    code:add_patha("/Users/romanshestakov/Development/erlang/ec/lib/digraphdot/ebin"),
    G = mdigraph:new(),
    Vertices = ["A", "B", "C", "D", "E", "F"],
    Edges =  [{"A", "B"}, {"A", "C"}, {"B", "D"}, {"C", "D"}, {"D", "E"}, {"E", "F"}],
    [mdigraph:add_vertex(G, V) || V <- Vertices],
    [mdigraph:add_edge(G, V1, V2) || {V1, V2} <- Edges],
    G.

-spec write_dot(mdigraph()) -> true.
write_dot(G) ->
    V = mdigraph_utils:topsort(G),
    Dot_IO_List = write_dot(V, [["digraph ", "test", "{", "ratio =", ?QUOTED("auto"),  "\n"]], G),   
    %%Tmp = erlang:phash2(make_ref()),
    erlang:iolist_to_binary(Dot_IO_List),
    file:write_file("graph52.dot", Dot_IO_List).

write_dot([], AllNodes, _G) ->
    [AllNodes | "}\n"];
write_dot([V | T], AllNodes,  G) ->
    Neighbours = mdigraph:out_neighbours(G, V),
    Nodes = lists:foldl(fun(V1, Acc) -> [?QUOTED(V), "->", ?QUOTED(V1), ";\n" | Acc] end, [], Neighbours),
    write_dot(T, [AllNodes | Nodes], G).


%%add_graph_attibutes(

%%output_to_pdf(F) ->
    
%% -spec get_ps(mdigraph()) -> any().
%% %% return graph in pdf format 
get_ps(G) ->
    D =  write_dot(G),
    P = start(),
    P ! {call, self(), D},
    receive
	{P, Result} ->
	    Result
    end,
    stop(P).


start() ->
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  Port = open_port({spawn, "dot -Tps -o test.ps"}, [stream]),
		  io:format("opened port"),
		  loop(Port)
	  end).

stop(P) ->
    P ! stop.

loop(Port) ->
     receive
	 {call, Caller, Msg} ->
	     io:format("got date ~p ~n", [Msg]),
	     Port ! {self(), {command, Msg}},
	     receive
		 {Port, {data, Data}} ->
		     io:format("got from port ~p ~n", [Data]),
		     Caller ! {self(), Data}
	     end,
	     loop(Port);
	 stop ->
	     Port ! {self() , close},
	     receive
		 {Port, closed} ->
		     exit(normal)
	     end;
	 {'EXIT', Port, Reason} ->
	     exit({port_terminated, Reason})
     end.
    







