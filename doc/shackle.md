

# Module shackle #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-4">call/4</a></td><td></td></tr><tr><td valign="top"><a href="#cast-4">cast/4</a></td><td></td></tr><tr><td valign="top"><a href="#receive_response-3">receive_response/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_pool-2">start_pool/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop_pool-2">stop_pool/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-4"></a>

### call/4 ###

<pre><code>
call(Namespace::module(), Msg::term(), Timeout::pos_integer(), PoolSize::pos_integer()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="cast-4"></a>

### cast/4 ###

<pre><code>
cast(Namespace::module(), Msg::term(), Pid::pid(), PoolSize::pos_integer()) -&gt; {ok, reference()} | {error, term()}
</code></pre>
<br />

<a name="receive_response-3"></a>

### receive_response/3 ###

<pre><code>
receive_response(Namespace::module(), Ref::reference(), Timeout::pos_integer()) -&gt; {ok, reference()} | {error, term()}
</code></pre>
<br />

<a name="start_pool-2"></a>

### start_pool/2 ###

<pre><code>
start_pool(Module::module(), PoolSize::pos_integer()) -&gt; [{ok, pid()}]
</code></pre>
<br />

<a name="stop_pool-2"></a>

### stop_pool/2 ###

<pre><code>
stop_pool(Module::module(), PoolSize::pos_integer()) -&gt; [ok | {error, atom()}]
</code></pre>
<br />

