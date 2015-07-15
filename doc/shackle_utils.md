

# Module shackle_utils #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#child_name-2">child_name/2</a></td><td></td></tr><tr><td valign="top"><a href="#child_names-2">child_names/2</a></td><td></td></tr><tr><td valign="top"><a href="#info_msg-2">info_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-3">lookup/3</a></td><td></td></tr><tr><td valign="top"><a href="#timeout-2">timeout/2</a></td><td></td></tr><tr><td valign="top"><a href="#warning_msg-2">warning_msg/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="child_name-2"></a>

### child_name/2 ###

<pre><code>
child_name(Module::module(), N::pos_integer()) -&gt; atom()
</code></pre>
<br />

<a name="child_names-2"></a>

### child_names/2 ###

<pre><code>
child_names(Module::module(), PoolSize::pos_integer()) -&gt; [atom()]
</code></pre>
<br />

<a name="info_msg-2"></a>

### info_msg/2 ###

<pre><code>
info_msg(Format::string(), Data::[term()]) -&gt; ok
</code></pre>
<br />

<a name="lookup-3"></a>

### lookup/3 ###

<pre><code>
lookup(Key::atom(), List::[{atom(), term()}], Default::term()) -&gt; term()
</code></pre>
<br />

<a name="timeout-2"></a>

### timeout/2 ###

<pre><code>
timeout(Timeout::pos_integer(), Timestamp::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>) -&gt; integer()
</code></pre>
<br />

<a name="warning_msg-2"></a>

### warning_msg/2 ###

<pre><code>
warning_msg(Format::string(), Data::[term()]) -&gt; ok
</code></pre>
<br />

