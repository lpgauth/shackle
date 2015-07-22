

# Module shackle_pool #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-pool_opt">pool_opt()</a> ###


<pre><code>
pool_opt() = {backlog_size, pos_integer()} | {client, module()} | {pool_size, pos_integer()} | {pool_strategy, <a href="#type-pool_strategy">pool_strategy()</a>}
</code></pre>




### <a name="type-pool_opts">pool_opts()</a> ###


<pre><code>
pool_opts() = [<a href="#type-pool_opt">pool_opt()</a>]
</code></pre>




### <a name="type-pool_strategy">pool_strategy()</a> ###


<pre><code>
pool_strategy() = random | round_robin
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#server-1">server/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; shackle_pool
</code></pre>
<br />

<a name="server-1"></a>

### server/1 ###

<pre><code>
server(Name::atom()) -&gt; {ok, pid()} | {error, backlog_full | shackle_not_started | pool_not_started}
</code></pre>
<br />

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(Name::atom(), Client::module()) -&gt; ok | {error, shackle_not_started | pool_already_started}
</code></pre>
<br />

<a name="start-3"></a>

### start/3 ###

<pre><code>
start(Name::atom(), Client::module(), PoolOpts::<a href="#type-pool_opts">pool_opts()</a>) -&gt; ok | {error, shackle_not_started | pool_already_started}
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Name::atom()) -&gt; ok | {error, shackle_not_started | pool_not_started}
</code></pre>
<br />

