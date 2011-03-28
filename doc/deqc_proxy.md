Module deqc_proxy
=================


<h1>Module deqc_proxy</h1>

* [Function Index](#index)
* [Function Details](#functions)






__Behaviours:__ [`gen_server`](gen_server.md).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#disconnect-1">disconnect/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td></td></tr><tr><td valign="top"><a href="#rpc-4">rpc/4</a></td><td></td></tr><tr><td valign="top"><a href="#sname-1">sname/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-2">stop/2</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="code_change-3"></a>

<h3>code_change/3</h3>





`code_change(FromVsn, St, Extra) -> any()`

<a name="disconnect-1"></a>

<h3>disconnect/1</h3>





`disconnect(Node) -> any()`

<a name="handle_call-3"></a>

<h3>handle_call/3</h3>





`handle_call(X1, From, St) -> any()`

<a name="handle_cast-2"></a>

<h3>handle_cast/2</h3>





`handle_cast(Msg, St) -> any()`

<a name="handle_info-2"></a>

<h3>handle_info/2</h3>





`handle_info(Msg, St) -> any()`

<a name="init-1"></a>

<h3>init/1</h3>





`init(X1) -> any()`

<a name="list-0"></a>

<h3>list/0</h3>





`list() -> any()`

<a name="rpc-4"></a>

<h3>rpc/4</h3>





`rpc(Node, M, F, A) -> any()`

<a name="sname-1"></a>

<h3>sname/1</h3>





`sname(N) -> any()`

<a name="start-2"></a>

<h3>start/2</h3>





`start(Node, Options) -> any()`

<a name="stop-2"></a>

<h3>stop/2</h3>





`stop(Node, How) -> any()`

<a name="terminate-2"></a>

<h3>terminate/2</h3>





`terminate(Reason, St) -> any()`

