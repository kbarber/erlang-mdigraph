%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(mdigraph).

%% to test:
%%mnesia:start().
%% Edges =  [{"A", "B"}, {"A", "C"}, {"B", "D"}, {"C", "D"}, {"D", "E"}, {"E", "F"}].
%% Vertices = ["A", "B", "C", "D", "E", "F"].
%% G = mdigraph:new().
%% [mdigraph:add_vertex(G, V) || V <- Vertices].
%% [mdigraph:add_edge(G, V1, V2) || {V1, V2} <- Edges].
%% mdigraph:del_edge(G, ['$e'|0]).
%%-include_lib("stdlib/include/qlc.hrl").

-export([new/0, new/1, delete/1, info/1]).
-export([add_vertex/1, add_vertex/2, add_vertex/3]).
-export([del_vertex/2, del_vertices/2]).
-export([vertex/2, no_vertices/1, vertices/1]).
-export([source_vertices/1, sink_vertices/1]).

-export([add_edge/3, add_edge/4, add_edge/5]).
-export([del_edge/2, del_edges/2, del_path/3]).
-export([edge/2, no_edges/1, edges/1]).

-export([out_neighbours/2, in_neighbours/2]).
-export([out_edges/2, in_edges/2, edges/2]).
-export([out_degree/2, in_degree/2]).
-export([get_path/3, get_cycle/2]).
-export([get_short_path/3, get_short_cycle/2]).

-export_type([mdigraph/0, d_type/0, vertex/0]).

-record(mdigraph, {vtab = notable :: mnesia:tab(),
		   etab = notable :: mnesia:tab(),
		   ntab = notable :: mnesia:tab(),
		   cyclic = true  :: boolean()}).
%% A declaration equivalent to the following one is hard-coded in erl_types.
%% That declaration contains hard-coded information about the #digraph{}
%% record and the types of its fields.  So, please make sure that any
%% changes to its structure are also propagated to erl_types.erl.
%%
-opaque mdigraph() :: #mdigraph{}.

-type edge()    :: term().
-type label()   :: term().
-type vertex()  :: term().
-type add_edge_err_rsn() :: {'bad_edge', [vertex()]} | {'bad_vertex', vertex()}.

%-record(vertex, {name, label}).
-record(edge, {edge, in, out, label}).
-record(neighbour, {name, edge}).
%%
%% Type is a list of
%%  protected | private
%%  acyclic | cyclic
%%
%%  default is [cyclic,protected]
%%
-type d_protection() :: 'private' | 'protected'. %% protection level is not applicable for mnesia, left for compatibility
-type d_cyclicity()  :: 'acyclic' | 'cyclic'.
-type d_type()       :: d_cyclicity() | d_protection().

%% CT covered
-spec new() -> mdigraph().
new() -> new([]).

-spec new([d_type()]) -> mdigraph().
new(Type) ->
    new(get_random_string(10, "abcdef01234567890"), Type).

new(Name, Type) ->
    case check_type(Type, protected, []) of
	{_Access, Ts} ->
	    V = list_to_atom("vertices-" ++ Name),
	    E = list_to_atom("edges-" ++ Name),
	    N = list_to_atom("neighbours-" ++ Name),
	    mnesia:create_table(V, [{type,set}]),
	    mnesia:create_table(E, [{type,set}, {attributes, record_info(fields, edge)}]),
	    mnesia:create_table(N, [{type,bag}, {attributes, record_info(fields, neighbour)}]),
	    Fun = fun() ->
			  mnesia:write({N, '$vid', 0}),
			  mnesia:write({N, '$eid', 0})
		  end,
	    {atomic, _} = mnesia:transaction(Fun),
	    set_type(Ts, #mdigraph{vtab=V, etab=E, ntab=N});
	error ->
	    erlang:error(badarg)
    end.

%% generate a random string to be used in tables name
-spec get_random_string(integer(), string() ) -> [].
get_random_string(Length, AllowedChars) ->
    %% set seed for random genarator
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).

%%
%% Check type of graph
%%
-spec check_type([d_type()], d_protection(), [{'cyclic', boolean()}]) ->
      	{d_protection(), [{'cyclic', boolean()}]}.
check_type([acyclic|Ts], A, L) ->
    check_type(Ts, A,[{cyclic,false} | L]);
check_type([cyclic | Ts], A, L) ->
    check_type(Ts, A, [{cyclic,true} | L]);
check_type([protected | Ts], _, L) ->
    check_type(Ts, protected, L);
check_type([private | Ts], _, L) ->
    check_type(Ts, private, L);
check_type([], A, L) -> {A, L};
check_type(_, _, _) -> error.


%%
%% Set graph type
%%
-spec set_type([{'cyclic', boolean()}], mdigraph()) -> mdigraph().
set_type([{cyclic,V} | Ks], G) ->
    set_type(Ks, G#mdigraph{cyclic = V});
set_type([], G) -> G.


%% Data access functions

%% CT covered
-spec delete(mdigraph()) -> 'true' | {aborted, any()}.
delete(G) ->
    case 
	begin
	    mnesia:delete_table(G#mdigraph.vtab),
	    mnesia:delete_table(G#mdigraph.etab),
	    mnesia:delete_table(G#mdigraph.ntab)
	end of
	{atomic, ok} -> true;
	{aborted, Reason} -> {aborted, Reason}
    end.


%% EU covered
-spec info(mdigraph()) -> [{'cyclicity', d_cyclicity()} |
			   {'memory', non_neg_integer()} |
			   {'protection', d_protection()}].
info(G) ->
    VT = G#mdigraph.vtab,
    ET = G#mdigraph.etab,
    NT = G#mdigraph.ntab,
    Cyclicity =
	case G#mdigraph.cyclic of
	    true  -> cyclic;
	    false -> acyclic
	end,
    Protection = protected,     % TODO: Fake a protection response for now
    Memory = mnesia:table_info(VT, memory) + mnesia:table_info(ET, memory) + mnesia:table_info(NT, memory),
    [{cyclicity, Cyclicity}, {memory, Memory}, {protection, Protection}].

%% CT covered
-spec add_vertex(mdigraph()) -> vertex().
add_vertex(G) ->
    do_add_vertex({new_vertex_id(G), []}, G).

-spec add_vertex(mdigraph(), vertex()) -> vertex().
add_vertex(G, V) ->
    do_add_vertex({V, []}, G).

-spec add_vertex(mdigraph(), vertex(), label()) -> vertex().
add_vertex(G, V, D) ->
    do_add_vertex({V, D}, G).

-spec del_vertex(mdigraph(), vertex()) -> 'true' | {abort, Reason::any()}.
del_vertex(G, V) ->
    case do_del_vertex(V, G) of
	{atomic, ok} ->
	    true;
	{abort, Reason} ->
	    {abort, Reason}
    end.

%% CT
-spec del_vertices(mdigraph(), [vertex()]) -> 'true'.
del_vertices(G, Vs) -> 
    do_del_vertices(Vs, G).

%% CT
-spec vertex(mdigraph(), vertex()) -> {vertex(), label()} | 'false'.
vertex(G, V) ->
    Fun = 
	fun() ->
		case mnesia:read(G#mdigraph.vtab, V) of
		    [] -> false;
		    [{_Tbl, Vertex, Label}] -> {Vertex, Label}
		end
	end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

%% CT
-spec no_vertices(mdigraph()) -> non_neg_integer().
no_vertices(G) ->
    mnesia:table_info(G#mdigraph.vtab, size).

% CT
-spec vertices(mdigraph()) -> [vertex()].
vertices(G) ->
    Fun = fun()-> mnesia:select(G#mdigraph.vtab, [{{'_', '$1', '_'}, [], ['$1']}]) end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

%% CT
-spec source_vertices(mdigraph()) -> [vertex()].
source_vertices(G) ->
    collect_vertices(G, in).

%% CT
-spec sink_vertices(mdigraph()) -> [vertex()].
sink_vertices(G) ->
    collect_vertices(G, out).

%% CT
-spec in_degree(mdigraph(), vertex()) -> non_neg_integer().
in_degree(G, V) ->
    degree(G, V, in).

%% CT
-spec out_degree(digraph(), vertex()) -> non_neg_integer().
out_degree(G, V) ->
    degree(G, V, out).

degree(G, V, InOrOut) ->
    Fun = fun() -> mnesia:read(G#mdigraph.ntab, {InOrOut, V}) end,
    {atomic, A} = mnesia:transaction(Fun),
    length(A).

%% CT
-spec in_neighbours(mdigraph(), vertex()) -> [vertex()].
in_neighbours(G, V) ->
    neighbours(G, V, in, 3).

%% CT
-spec out_neighbours(mdigraph(), vertex()) -> [vertex()].
out_neighbours(G, V) ->
    neighbours(G, V, out, 4).

neighbours(G, V, InOrOut, Index) ->
    ET = G#mdigraph.etab,
    NT = G#mdigraph.ntab,
    Fun = fun() -> mnesia:read(NT, {InOrOut, V}) end,
    {atomic, A} = mnesia:transaction(Fun),
    collect_elems(A, ET, Index).

%% CT
-spec in_edges(mdigraph(), vertex()) -> [edge()].
in_edges(G, V) ->
    Fun = fun() -> mnesia:select(G#mdigraph.ntab, [{{'$1', {in, V}, '$2'}, [], ['$2']}]) end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

%% CT
-spec out_edges(mdigraph(), vertex()) -> [edge()].
out_edges(G, V) ->
    Fun = fun() -> mnesia:select(G#mdigraph.ntab, [{{'$1', {out, V}, '$2'}, [], ['$2']}]) end,
    {atomic,Result} = mnesia:transaction(Fun),
    Result.

%%CT
-spec add_edge(mdigraph(), vertex(), vertex()) ->
	 edge() | {'error', add_edge_err_rsn()}.
add_edge(G, V1, V2) ->
    do_add_edge({new_edge_id(G), V1, V2, []}, G).


-spec add_edge(mdigraph(), vertex(), vertex(), label()) ->
	 edge() | {'error', add_edge_err_rsn()}.
add_edge(G, V1, V2, D) ->
    do_add_edge({new_edge_id(G), V1, V2, D}, G).


-spec add_edge(mdigraph(), edge(), vertex(), vertex(), label()) ->
	 edge() | {'error', add_edge_err_rsn()}.
add_edge(G, E, V1, V2, D) ->
    do_add_edge({E, V1, V2, D}, G).

%% CT
del_edge(G, E) ->
    do_del_edges([E], G).

-spec del_edges(mdigraph(), [edge()]) -> 'true'.
del_edges(G, Es) ->
    do_del_edges(Es, G).

-spec no_edges(digraph()) -> non_neg_integer().
no_edges(G) ->
    mnesia:table_info(G#mdigraph.etab, size).

%% CT
-spec edges(mdigraph()) -> [edge()].
edges(G) ->
    Fun = fun()-> mnesia:select(G#mdigraph.etab, [{{'_', '$1', '_', '_', '_'}, [], ['$1']}]) end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.


-spec edges(mdigraph(), vertex()) -> [edge()].
edges(G, V) ->
    Fun = fun()->
        mnesia:select(G#mdigraph.ntab, [{{'_',{out, V},'$1'}, [], ['$1']},
				{{{in, V}, '$1'}, [], ['$1']}])
    end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

-spec edge(mdigraph(), edge()) -> {edge(),vertex(),vertex(),label()} | 'false'.
edge(G, E) ->
    Fun = fun()->
        mnesia:read(G#mdigraph.etab,E)
    end,
    {atomic, A} = mnesia:transaction(Fun),

    case A of
        [] -> false;
        [{_, Edge, V1, V2, Label}] -> {Edge, V1, V2, Label}
    end.

-spec new_edge_id(mdigraph()) -> nonempty_improper_list('$e', non_neg_integer()).
new_edge_id(G) ->
    ['$e' | get_id(G, '$eid')].

-spec new_vertex_id(mdigraph()) -> nonempty_improper_list('$v', non_neg_integer()).
new_vertex_id(G) ->
    ['$v' | get_id(G, '$vid')].

get_id(G, Id) ->
    Fun = fun() ->
        NT = G#mdigraph.ntab,
        [{Tab, Id, K}] = mnesia:read(NT, Id),
        ok = mnesia:delete_object(NT, {Tab, Id, K}, write),
        ok = mnesia:write({NT, Id, K + 1}),
        K
    end,
    {atomic, Result} = mnesia:transaction(Fun),
    Result.

collect_elems(Keys, Table, Index) ->
    collect_elems(Keys, Table, Index, []).

collect_elems([{_, _, Key} | Keys], Table, Index, Acc) ->
    collect_elems(Keys, Table, Index,
		  [ets:lookup_element(Table, Key, Index) |Acc]);
collect_elems([], _, _, Acc) -> Acc.

-spec do_add_vertex({vertex(), label()}, mdigraph()) -> vertex().
do_add_vertex({V, Label}, G) ->
    Fun = fun()->
		  mnesia:write(G#mdigraph.vtab, {G#mdigraph.vtab, V, Label}, write)
	  end,
    mnesia:transaction(Fun),
    V.

%%
%% Collect either source or sink vertices.
%%
collect_vertices(G, Type) ->
    Vs = vertices(G),
    lists:foldl(fun(V, A) ->
        T = mnesia:transaction(fun() ->
            mnesia:read({G#mdigraph.ntab, {Type,V}})
        end),
        case T of
            {atomic, []} -> [V|A];
            {atomic, [_|_]} -> A
        end
    end, [], Vs).


do_del_vertices([V | Vs], G) ->
    do_del_vertex(V, G),
    do_del_vertices(Vs, G);
do_del_vertices([], #mdigraph{}) -> true.

do_del_vertex(V, G) ->
    {atomic, E1} = mnesia:transaction(fun() -> 
        mnesia:read({G#mdigraph.ntab, {in, V}}) 
    end),
    do_del_nedges(E1, G),

    {atomic, E2} = mnesia:transaction(fun() -> 
        mnesia:read({G#mdigraph.ntab, {out, V}}) 
    end),
    do_del_nedges(E2, G),

    mnesia:transaction(fun() ->
        mnesia:delete({G#mdigraph.vtab, V})
    end).

do_del_nedges([{_, _, E}|Ns], G) ->
    {atomic, R} = mnesia:transaction(fun() ->
        mnesia:read({G#mdigraph.etab, E})
    end),
    case R of
        [{_, E, V1, V2, _}] ->
            do_del_edge(E, V1, V2, G),
            do_del_nedges(Ns, G);
        [] -> % cannot happen
            do_del_nedges(Ns, G)
    end;
do_del_nedges([], #mdigraph{}) -> true.

%%
%% Delete edges
%%
do_del_edges([E|Es], G) ->
    case ets:lookup(G#mdigraph.etab, E) of
	[{_,E,V1,V2,_}] ->
	    do_del_edge(E,V1,V2,G),
	    do_del_edges(Es, G);
	[] ->
	    do_del_edges(Es, G)
    end;
do_del_edges([], #mdigraph{}) -> true.

do_del_edge(E, _V1, _V2, G) ->
    {atomic, Result} =
	mnesia:transaction(
	  fun() ->
		  A = mnesia:select(G#mdigraph.ntab, [{{'$1','$2', E}, [], [{{'$1','$2'}}]}], write),
		  lists:foreach(fun(R) -> mnesia:delete(R) end, A),
		  [ER] = mnesia:read({G#mdigraph.etab, E}),
		  mnesia:delete_object(ER)
	  end),
    Result.


-spec rm_edges([vertex(),...], mdigraph()) -> 'true'.
rm_edges([V1, V2|Vs], G) ->
    rm_edge(V1, V2, G),
    rm_edges([V2|Vs], G);
rm_edges(_, _) -> true.

-spec rm_edge(vertex(), vertex(), mdigraph()) -> 'ok'.
rm_edge(V1, V2, G) ->
    Es = out_edges(G, V1),
    rm_edge_0(Es, V1, V2, G).
    
rm_edge_0([E|Es], V1, V2, G) ->
    case ets:lookup(G#mdigraph.etab, E) of
	[{E, V1, V2, _}]  ->
            do_del_edge(E, V1, V2, G),
	    rm_edge_0(Es, V1, V2, G);
	_ ->
	    rm_edge_0(Es, V1, V2, G)
    end;
rm_edge_0([], _, _, #mdigraph{}) -> ok.

%%
%% Check that endpoints exist
%%
-spec do_add_edge({edge(), vertex(), vertex(), label()}, mdigraph()) ->
	edge() | {'error', add_edge_err_rsn()}.
do_add_edge({E, V1, V2, Label}, G) ->
    case ets:member(G#mdigraph.vtab, V1) of
	false -> erlang:error({bad_vertex, V1}); %%{error, {bad_vertex, V1}};
	true  ->
	    case ets:member(G#mdigraph.vtab, V2) of
		false -> erlang:error({bad_vertex, V2}); %% {error, {bad_vertex, V2}};
                true ->
                    case other_edge_exists(G, E, V1, V2) of
                        true -> erlang:error({bad_edge, [V1, V2]});%%{error, {bad_edge, [V1, V2]}};
                        false when G#mdigraph.cyclic =:= false ->
                            acyclic_add_edge(E, V1, V2, Label, G);
                        false ->
                            do_insert_edge(E, V1, V2, Label, G)
                    end
	    end
    end.


other_edge_exists(#mdigraph{etab = ET}, E, V1, V2) ->
    case ets:lookup(ET, E) of
        [{E, Vert1, Vert2, _}] when Vert1 =/= V1; Vert2 =/= V2 ->
            true;
        _ ->
            false
    end.


-spec do_insert_edge(edge(), vertex(), vertex(), label(), mdigraph()) -> edge().
do_insert_edge(E, V1, V2, Label, #mdigraph{ntab=NT, etab=ET}) ->
    %%Edge_row = {ET, E, V1, V2, Label},
    Fun = fun() ->
        mnesia:write({NT, {out, V1}, E}),
        mnesia:write({NT, {in, V2}, E}),
        mnesia:write({ET, E, V1, V2, Label})
    end,
    {atomic, _} = mnesia:transaction(Fun),
    E.

-spec acyclic_add_edge(edge(), vertex(), vertex(), label(), mdigraph()) ->
	edge() | {'error', {'bad_edge', [vertex()]}}.
acyclic_add_edge(_E, V1, V2, _L, _G) when V1 =:= V2 ->
    {error, {bad_edge, [V1, V2]}};
acyclic_add_edge(E, V1, V2, Label, G) ->
    case get_path(G, V2, V1) of
	false -> do_insert_edge(E, V1, V2, Label, G);
	Path -> {error, {bad_edge, Path}}
    end.

-spec del_path(mdigraph(), vertex(), vertex()) -> 'true'.
del_path(G, V1, V2) ->
    case get_path(G, V1, V2) of
	false -> true;
	Path ->
	    rm_edges(Path, G),
	    del_path(G, V1, V2)
    end.

-spec get_cycle(mdigraph(), vertex()) -> [vertex(),...] | 'false'.
get_cycle(G, V) ->
    case one_path(out_neighbours(G, V), V, [], [V], [V], 2, G, 1) of
	false ->
	    case lists:member(V, out_neighbours(G, V)) of
		true -> [V];
		false -> false
	    end;
	Vs -> Vs
    end.

%% CT
-spec get_path(digraph(), vertex(), vertex()) -> [vertex(),...] | 'false'.
get_path(G, V1, V2) ->
    one_path(out_neighbours(G, V1), V2, [], [V1], [V1], 1, G, 1).

%%
%% prune_short_path (evaluate conditions on path)
%% short : if path is too short
%% ok    : if path is ok
%%
prune_short_path(Counter, Min) when Counter < Min ->
    short;
prune_short_path(_Counter, _Min) ->
    ok.

one_path([W|Ws], W, Cont, Xs, Ps, Prune, G, Counter) ->
    case prune_short_path(Counter, Prune) of
	short -> one_path(Ws, W, Cont, Xs, Ps, Prune, G, Counter);
	ok -> lists:reverse([W|Ps])
    end;
one_path([V|Vs], W, Cont, Xs, Ps, Prune, G, Counter) ->
    case lists:member(V, Xs) of
	true ->  one_path(Vs, W, Cont, Xs, Ps, Prune, G, Counter);
	false -> one_path(out_neighbours(G, V), W, 
			  [{Vs,Ps} | Cont], [V|Xs], [V|Ps], 
			  Prune, G, Counter+1)
    end;
one_path([], W, [{Vs,Ps}|Cont], Xs, _, Prune, G, Counter) ->
    one_path(Vs, W, Cont, Xs, Ps, Prune, G, Counter-1);
one_path([], _, [], _, _, _, _, _Counter) -> false.


%%
%% Like get_cycle/2, but a cycle of length one is preferred.
%%
-spec get_short_cycle(mdigraph(), vertex()) -> [vertex(),...] | 'false'.
get_short_cycle(G, V) ->
    get_short_path(G, V, V).

%%
%% Like get_path/3, but using a breadth-first search makes it possible
%% to find a short path.
%%
-spec get_short_path(mdigraph(), vertex(), vertex()) -> [vertex(),...] | 'false'.
get_short_path(G, V1, V2) ->
    T = new(),
    add_vertex(T, V1),
    Q = queue:new(),
    Q1 = queue_out_neighbours(V1, G, Q),
    L = spath(Q1, G, V2, T),
    delete(T),
    L.
    
spath(Q, G, Sink, T) ->
    case queue:out(Q) of
	{{value, E}, Q1} ->
	    {_E, V1, V2, _Label} = edge(G, E),
	    if 
		Sink =:= V2 ->
		    follow_path(V1, T, [V2]);
		true ->
		    case vertex(T, V2) of
			false ->
			    add_vertex(T, V2),
			    add_edge(T, V2, V1),
			    NQ = queue_out_neighbours(V2, G, Q1),
			    spath(NQ, G, Sink, T);
			_V ->
			    spath(Q1, G, Sink, T)
		    end
	    end;
	{empty, _Q1} ->
	    false
    end.

follow_path(V, T, P) ->
    P1 = [V | P],
    case out_neighbours(T, V) of
	[N] ->
	    follow_path(N, T, P1);
	[] ->
	    P1
    end.

queue_out_neighbours(V, G, Q0) ->
    lists:foldl(fun(E, Q) -> queue:in(E, Q) end, Q0, out_edges(G, V)).


