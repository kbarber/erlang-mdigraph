-module(mdigraphdot).
-compile([export_all]).

%%-type vertices() :: [mdigraph:vertex()].
-type mdigraph() :: [mdigraph:mdigraph()].

start() ->
    register(dot, spawn(fun() -> loop([]) end)).
stop() ->
    dot ! stop.

-spec write_dot(mdigraph()) -> true.
write_dot(G) ->
    V = mdigraph_utils:topsort(G),
    write_dot(V, G).

write_dot([], _G) ->
    R = get_result(),
    dot ! {self(), reset},
    R;
write_dot([V | T], G) ->
    Neighbours = mdigraph:out_neighbours(G, V),
    lists:foreach(fun(V1) -> send(V, V1) end, Neighbours),
    write_dot(T, G).

get_result() ->
    dot ! {self(), get_result},
    receive 
	{dot, L} ->
	    L
    end.

send(V, V1) ->
    dot ! {self(), {node, V, V1}},
    receive
	{dot, Reply} ->
	    Reply
    end.
    

loop(L) ->
    receive
	{From, {node, V, V1}} ->
	    L1 = [lists:concat([V, " -> ", V1, "\n"]) | L],
	    From ! {dot, true},
	    loop(L1);
	stop ->
	    void;
	{From, get_result} ->
	    From ! {dot, L},
	    loop(L);
	{_From, reset} ->
	    loop([]);
	Other ->
	    io:format("unexpected ~p ~n", [Other]),
	    loop(L)
    end.
