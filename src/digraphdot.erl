-module(digraphdot).
%%-compile([export_all]).

-export([get_ps/1, 
	 init_graph/0]).

-define(QUOTED(Val), $",Val,$"). 
-define(EDGE(V1, V2), [?QUOTED(V1), "->", ?QUOTED(V2), ";\n"]).
-define(ATTR(V1, V2), [atom_to_list(V1), "=", V2, ";\n"]).

%%-type vertices() :: [mdigraph:vertex()].
-type mdigraph() :: [mdigraph:mdigraph()].

-record(graph_attributes, {ratio = "auto", ranksep = ".75"}).

record_to_proplist(#graph_attributes{} = Rec) ->
  lists:zip(record_info(fields, graph_attributes), tl(tuple_to_list(Rec))).

%% dot -Tps graph54.dot -o graph54.ps


init_graph() ->
    code:add_patha("/Users/romanshestakov/Development/erlang/ec/lib/digraphdot/ebin"),
    mnesia:start(),
    G = mdigraph:new(),
    Vertices = ["A", "B", "C", "D", "E", "F"],
    Edges =  [{"A", "B"}, {"A", "C"}, {"B", "D"}, {"C", "D"}, {"D", "E"}, {"E", "F"}],
    [mdigraph:add_vertex(G, V) || V <- Vertices],
    [mdigraph:add_edge(G, V1, V2) || {V1, V2} <- Edges],
    G.

-spec generate_dot(mdigraph()) -> true.
generate_dot(G) ->
    Graph = build_graph(G),
    %%Tmp = erlang:phash2(make_ref()),
    Dot_IO_List = write_dot(Graph),
    erlang:iolist_to_binary(Dot_IO_List).
    %% file:write_file("graph54.dot", Dot_IO_List).


write_dot({{graph, {name, Name}, {attributes, Attrb}, {edges, Edges}}}) ->
    Acc = write_dot({name, Name}, []),
    Acc1 = write_dot({attributes, Attrb}, Acc),
    write_dot({edges, Edges}, Acc1).


write_dot({name, Name}, Acc) ->
    [["digraph ", Name, "{\n"] | Acc];
write_dot({attributes, Attrb}, Acc) ->
    write_attr(record_to_proplist(Attrb), Acc);
write_dot({edges, Edges}, Acc) ->
    write_edges(Edges, Acc).


%% writes edges
write_edges([], Acc) ->
    lists:reverse(["}\n" | Acc]);
write_edges([{V1, V2} | T], Acc) ->
    Row = ?EDGE(V1, V2),
    write_edges(T, [Row | Acc]).

%% writes edges
write_attr([], Acc) ->
    Acc;
write_attr([{V1, V2} | T], Acc) ->
    Row = ?ATTR(V1, V2),
    write_attr(T, [Row | Acc]).


%% build a graph representation from mdigraph or digraph
-spec(build_graph(mdigraph() | digraph()) -> {{graph, {name}, {attributes}, {edges, []}}}).
build_graph(G) ->
    E = [get_node(mdigraph:edge(G, E)) || E <- mdigraph:edges(G)],
    {{graph, {name, get_graph_name(G)}, {attributes, #graph_attributes{}}, {edges, E}}}.

%% helper function used by build_graph  
get_node({_E, V1, V2, _L}) ->
    {V1, V2}.



%% find out the type of the graph
-spec graph_type(mdigraph() | digraph()) -> mdigraph | digraph.	     
graph_type(G)->    
    case element(1, G) of
	mdigraph ->
	    mdigraph;
	digraph ->
	    digraph
    end.
    

get_graph_name(G) ->
    case graph_type(G) of
	mdigraph ->
	    "mdigraph";
	digraph ->
	    "digraph"
    end.




%%add_graph_attibutes(

%%output_to_pdf(F) ->
    
%% -spec get_ps(mdigraph()) -> any().
%% %% return graph in pdf format 
get_ps(G) ->
    Dot =  generate_dot(G),
    Pid = start(),
    Pid ! {call, self(), Dot}.
%%    stop(Pid).


start() ->
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  Port = open_port({spawn, "dot -Tps -O"}, [stream]),
		  loop(Port)
	  end).

%% stop(P) ->
%%     P ! stop.

loop(Port) ->
     receive
	 {call, _Caller, Msg} ->
	     %%io:format("got data to send to port ~p ~n", [Msg]),
	     Port ! {self(), {command, Msg}}
	     %% receive
	     %% 	 {Port, {data, Data}} ->
	     %% 	     io:format("got reply from port ~p ~n", [Data]),
	     %% 	     Caller ! {self(), Data}
	     %% end,
	     %% loop(Port);
	 %% stop ->
 	 %%     io:format("closing port ~n"),
	 %%     Port ! {self(), close},
	 %%     receive
	 %% 	 {Port, closed} ->
	 %% 	     exit(normal)
	 %%     end;
	 %% {'EXIT', Port, Reason} ->
	 %%     exit({port_terminated, Reason})
     end.







