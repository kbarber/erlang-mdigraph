-module(digraphdot).
%%-compile([export_all]).

-export([get_svg/1, 
	 init_graph/0, 
	 generate_dot/1]).

-define(QUOTED(Val), [$",Val,$"]). 
-define(EDGE(V1, V2), [?QUOTED(V1), "->", ?QUOTED(V2), ";\n"]).
-define(ATTR(V1, V2), [atom_to_list(V1), "=", V2, ";\n"]).

%%-define(SHELL, "/bin/sh -s unix:cmd dot -Tsvg 2>&1").
-define(SHELL, "dot -Tsvg").
-define(PORT_CREATOR_NAME, os_cmd_port_creator).

-define(C, ["C~0","C~1","C~2","C~3","C~4","C~5","C~6","C~7","C~8","C~9","C~10","C~11","C~12","C~13","C~14","C~15","C~16","C~17","C~18","C~19","C~20","C~21","C~22","C~23","C~24","C~25","C~26","C~27","C~28","C~29","C~30","C~31","C~32","C~33","C~34","C~35","C~36","C~37","C~38","C~39","C~40","C~41","C~42","C~43","C~44","C~45","C~46","C~47","C~48","C~49","C~50","C~51","C~52","C~53","C~54","C~55","C~56","C~57","C~58","C~59"]).



%%-type vertices() :: [mdigraph:vertex()].
-type mdigraph() :: [mdigraph:mdigraph()].
%%[$","7.5, 7.5",$"]
-record(graph_attributes, {ratio = "compress", ranksep = ".75", size = ?QUOTED("7.5, 7.5")}).

record_to_proplist(#graph_attributes{} = Rec) ->
  lists:zip(record_info(fields, graph_attributes), tl(tuple_to_list(Rec))).

%% dot -Tps graph54.dot -o graph54.ps


init_graph() ->
    code:add_patha("/Users/romanshestakov/Development/erlang/ec/lib/digraphdot/ebin"),
    mnesia:start(),
    G = mdigraph:new(),
    Vertices = ["A", "B", "C", "D", "E", "F"] ++ ?C,
    Edges =  [{"A", "B"}, {"A", "C"}, {"B", "D"}, {"C", "D"}, {"D", "E"}, {"E", "F"}] ++ [{"C", V} || V <- ?C],
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
get_svg(G) ->
    Dot =  generate_dot(G),
    Cmd = "dot -Tsvg",
    Opt = [stream, exit_status, use_stdio, stderr_to_stdout, eof],
    Port = open_port({spawn, Cmd}, Opt),
    port_command(Port, Dot),
    get_data(Port, []).

get_data(P, D) ->
     receive
         {P, {data, D1}} ->
	     %%io:format("~p", [D1]),
             get_data(P, [D1 | D])
         %% {P, eof} ->
         %%     port_close(P),
         %%     receive
         %%         {P, {exit_status, N}} ->
         %%             {N, lists:reverse(D)}
         %%     end
     after 100 ->
	     port_close(P),
	     lists:reverse(D)
     end.

