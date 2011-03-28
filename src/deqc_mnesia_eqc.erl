-module(deqc_mnesia_eqc).
-compile(export_all).

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").


prop_mnesia_schema() ->
    ?FORALL(Ns, running_nodes(),
	    ?LET({N,_}, elements(Ns),
		 ok = create_schema(N, Ns))).


running_nodes() ->
    ?LET(Ns, subset(all_nodes()),
	 start_nodes(Ns)).

subset(L) ->
    ?LET(Keep, [ {bool(),G} || G<-L ],
	 [ G || {true,G}<-Keep ]).

all_nodes() ->
    [a,b,c].

start_nodes(Ns) ->
    Ns.

create_schema(FromNode, Nodes) ->
    RealNodes = [N || {_, N} <- Nodes],
    deqc_proxy:rpc(FromNode, mnesia, create_schema, [RealNodes]).

-endif.
