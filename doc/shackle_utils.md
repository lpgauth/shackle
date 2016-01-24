

# Module shackle_utils #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-backlog_size">backlog_size()</a> ###


<pre><code>
backlog_size() = pos_integer() | infinity
</code></pre>




### <a name="type-client_option">client_option()</a> ###


<pre><code>
client_option() = {ip, <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {protocol, <a href="#type-protocol">protocol()</a>} | {reconnect, boolean()} | {reconnect_time_max, <a href="#type-time">time()</a>} | {reconnect_time_min, <a href="#type-time">time()</a>} | {socket_options, [<a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a> | <a href="gen_udp.md#type-option">gen_udp:option()</a>]}
</code></pre>




### <a name="type-client_options">client_options()</a> ###


<pre><code>
client_options() = [<a href="#type-client_option">client_option()</a>]
</code></pre>




### <a name="type-pool_name">pool_name()</a> ###


<pre><code>
pool_name() = atom()
</code></pre>




### <a name="type-pool_option">pool_option()</a> ###


<pre><code>
pool_option() = {backlog_size, <a href="#type-backlog_size">backlog_size()</a>} | {pool_size, <a href="#type-pool_size">pool_size()</a>} | {pool_strategy, <a href="#type-pool_strategy">pool_strategy()</a>}
</code></pre>




### <a name="type-pool_options">pool_options()</a> ###


<pre><code>
pool_options() = [<a href="#type-pool_option">pool_option()</a>]
</code></pre>




### <a name="type-pool_size">pool_size()</a> ###


<pre><code>
pool_size() = pos_integer()
</code></pre>




### <a name="type-pool_strategy">pool_strategy()</a> ###


<pre><code>
pool_strategy() = random | round_robin
</code></pre>




### <a name="type-protocol">protocol()</a> ###


<pre><code>
protocol() = shackle_tcp | shackle_udp
</code></pre>




### <a name="type-time">time()</a> ###


<pre><code>
time() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cancel_timer-1">cancel_timer/1</a></td><td></td></tr><tr><td valign="top"><a href="#info_msg-3">info_msg/3</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-3">lookup/3</a></td><td></td></tr><tr><td valign="top"><a href="#random-1">random/1</a></td><td></td></tr><tr><td valign="top"><a href="#random_element-1">random_element/1</a></td><td></td></tr><tr><td valign="top"><a href="#timeout-2">timeout/2</a></td><td></td></tr><tr><td valign="top"><a href="#warning_msg-3">warning_msg/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cancel_timer-1"></a>

### cancel_timer/1 ###

<pre><code>
cancel_timer(TimerRef::undefined | reference()) -&gt; ok
</code></pre>
<br />

<a name="info_msg-3"></a>

### info_msg/3 ###

<pre><code>
info_msg(Pool::<a href="#type-pool_name">pool_name()</a>, Format::string(), Data::[term()]) -&gt; ok
</code></pre>
<br />

<a name="lookup-3"></a>

### lookup/3 ###

<pre><code>
lookup(Key::atom(), List::[{atom(), term()}], Default::term()) -&gt; term()
</code></pre>
<br />

<a name="random-1"></a>

### random/1 ###

<pre><code>
random(N::pos_integer()) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="random_element-1"></a>

### random_element/1 ###

<pre><code>
random_element(List::[term()]) -&gt; term()
</code></pre>
<br />

<a name="timeout-2"></a>

### timeout/2 ###

<pre><code>
timeout(Timeout::<a href="#type-time">time()</a>, Timestamp::<a href="erlang.md#type-timestamp">erlang:timestamp()</a>) -&gt; integer()
</code></pre>
<br />

<a name="warning_msg-3"></a>

### warning_msg/3 ###

<pre><code>
warning_msg(Pool::<a href="#type-pool_name">pool_name()</a>, Format::string(), Data::[term()]) -&gt; ok
</code></pre>
<br />

