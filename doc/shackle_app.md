

# Module shackle_app #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`application`](application.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; {ok, [atom()]}
</code></pre>
<br />

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(StartType::<a href="application.md#type-start_type">application:start_type()</a>, StartArgs::term()) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="stop-0"></a>

### stop/0 ###

<pre><code>
stop() -&gt; ok
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(State::term()) -&gt; ok
</code></pre>
<br />

