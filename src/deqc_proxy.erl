%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(deqc_proxy).
-behaviour(gen_server).

-include_lib("parse_trans/include/codegen.hrl").


-export([start/2,
         disconnect/1,
         stop/2,
	 rpc/4]).

-export([sname/1,
	 list/0]).


-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(st, {node,
	     alias,
	     os_pid,
	     options = []}).


start(Node, Options) ->
    gen_server:start_link(?MODULE, {Node, Options}, []).

disconnect(Node) ->            
    call(Node, disconnect).

stop(Node, How) ->
    call(Node, {stop, How}).

rpc(Node, M, F, A) ->
    call(Node, {rpc, M, F, A}).

list() ->
    [{N,V} || {{n,l,{?MODULE,N}},_,V} <-
	      gproc:select(n, [{ {{n,l,{?MODULE,'_'}},'_','_'}, [], ['$_']}])].


sname(N) ->
    case has_@(N) of
	true ->
	    N;
	false ->
	    {_, H} = split_node(node()),
	    list_to_atom(atom_to_list(N) ++ "@" ++ atom_to_list(H))
    end.


call(Node, Request) ->
    case gproc:lookup_local_name({?MODULE, Node}) of
	undefined ->
	    erlang:error(badarg, [Node, Request]);
	Pid when is_pid(Pid) ->
	    gen_server:call(Pid, Request)
    end.

handle_call(disconnect, _From, St) ->
    Res = rpc:call(St#st.node, erl_eval, exprs, [disconnect_script(node()), []]),
    {reply, Res, St};
handle_call({stop, kill}, _From, #st{os_pid = OsPid} = St) ->
    Res = os:cmd("kill -9 " ++ OsPid),
    {reply, Res, St};
handle_call({stop, How}, _From, St) ->
    {M,F,A} = case How of
		  shutdown -> {init, stop, []};
		  halt     -> {erlang, halt, []}
	      end,
    Res = rpc:call(St#st.node, M, F, A),
    {reply, Res, St};
handle_call({rpc, M, F, A}, _From, St) ->
    Res = rpc:call(St#st.node, M, F, A),
    {reply, Res, St}.
		   

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({nodedown, N}, #st{node = N} = St) ->
    {stop, normal, St};
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.



init({Node, Options}) ->
    try
	gproc:add_local_name({?MODULE, Node}),
	State0 = check_options(Node, Options),
	case do_start_node(State0) of
	    {ok, #st{node = NodeName} = State1} ->
		gproc:set_value({n,l,{?MODULE, Node}}, NodeName),
		{ok, State1};
	    Other ->
		Other
	end
    catch
	error:E ->
	    {error, E}
    end.

check_options(Node, Options) ->
    Nodename = nodename(Node, Options),
    #st{node = Nodename,
	alias = Node,
	options = Options}.

nodename(N, Options) ->
    case lists:keyfind(node, 1, Options) of
	{_, Name} ->
	    Name;
	false ->
	    sname(N)
    end.

do_start_node(St) ->
    {Name, Host} = split_node(St#st.node),
    case start_slave(Name, Host, St) of
	{ok, Nodename} ->
	    monitor_node(Nodename, true),
	    {ok, St#st{node = Nodename,
		       os_pid = getpid(Nodename)}};
	Other ->
	    Other
    end.

start_slave(Name, Host, _St) ->
    slave:start_link(Host, Name).

getpid(N) ->
    rpc:call(N, os, getpid, []).


split_node(N) ->
    [Name, Host] = re:split(atom_to_list(N), "@", [{return, list}]),
    {list_to_atom(Name), list_to_atom(Host)}.


has_@(N) when is_atom(N) ->
    lists:member($@, atom_to_list(N)).

disconnect_script(EqcNode) ->
    codegen:exprs(
      fun() ->
	      Nodes = nodes(),
	      [net_kernel:disconnect(N) || N <- Nodes -- [{'$var', EqcNode}]]
      end).
