

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




### <a name="type-cast">cast()</a> ###


<pre><code>
cast() = #cast{client = <a href="#type-client">client()</a>, pid = undefined | pid(), request = term(), request_id = <a href="#type-request_id">request_id()</a>, timeout = timeout(), timestamp = <a href="erlang.md#type-timestamp">erlang:timestamp()</a>}
</code></pre>




### <a name="type-client">client()</a> ###


<pre><code>
client() = module()
</code></pre>




### <a name="type-client_option">client_option()</a> ###


<pre><code>
client_option() = {ip, <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {protocol, <a href="#type-protocol">protocol()</a>} | {reconnect, boolean()} | {reconnect_time_max, <a href="#type-time">time()</a> | infinity} | {reconnect_time_min, <a href="#type-time">time()</a>} | {socket_options, [<a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a> | <a href="gen_udp.md#type-option">gen_udp:option()</a>]}
</code></pre>




### <a name="type-client_options">client_options()</a> ###


<pre><code>
client_options() = [<a href="#type-client_option">client_option()</a>]
</code></pre>




### <a name="type-client_state">client_state()</a> ###


<pre><code>
client_state() = term()
</code></pre>




### <a name="type-external_request_id">external_request_id()</a> ###


<pre><code>
external_request_id() = term()
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
protocol() = shackle_ssl | shackle_tcp | shackle_udp
</code></pre>




### <a name="type-reconnect_state">reconnect_state()</a> ###


<pre><code>
reconnect_state() = #reconnect_state{current = undefined | <a href="#type-time">time()</a>, max = <a href="#type-time">time()</a> | infinity, min = <a href="#type-time">time()</a>}
</code></pre>




### <a name="type-request_id">request_id()</a> ###


<pre><code>
request_id() = {<a href="#type-server_name">server_name()</a>, reference()}
</code></pre>




### <a name="type-response">response()</a> ###


<pre><code>
response() = {<a href="#type-external_request_id">external_request_id()</a>, term()}
</code></pre>




### <a name="type-server_name">server_name()</a> ###


<pre><code>
server_name() = atom()
</code></pre>




### <a name="type-time">time()</a> ###


<pre><code>
time() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cancel_timer-1">cancel_timer/1</a></td><td></td></tr><tr><td valign="top"><a href="#client_setup-3">client_setup/3</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-3">lookup/3</a></td><td></td></tr><tr><td valign="top"><a href="#process_responses-2">process_responses/2</a></td><td></td></tr><tr><td valign="top"><a href="#random-1">random/1</a></td><td></td></tr><tr><td valign="top"><a href="#random_element-1">random_element/1</a></td><td></td></tr><tr><td valign="top"><a href="#reconnect_state-1">reconnect_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#reconnect_state_reset-1">reconnect_state_reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#reply-3">reply/3</a></td><td></td></tr><tr><td valign="top"><a href="#reply_all-2">reply_all/2</a></td><td></td></tr><tr><td valign="top"><a href="#warning_msg-3">warning_msg/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cancel_timer-1"></a>

### cancel_timer/1 ###

<pre><code>
cancel_timer(TimerRef::undefined | reference()) -&gt; ok
</code></pre>
<br />

<a name="client_setup-3"></a>

### client_setup/3 ###

<pre><code>
client_setup(Client::<a href="#type-client">client()</a>, PoolName::<a href="#type-pool_name">pool_name()</a>, Socket::<a href="inet.md#type-socket">inet:socket()</a>) -&gt; {ok, <a href="#type-client_state">client_state()</a>} | {error, term(), <a href="#type-client_state">client_state()</a>}
</code></pre>
<br />

<a name="lookup-3"></a>

### lookup/3 ###

<pre><code>
lookup(Key::atom(), List::[{atom(), term()}], Default::term()) -&gt; term()
</code></pre>
<br />

<a name="process_responses-2"></a>

### process_responses/2 ###

<pre><code>
process_responses(T::[<a href="#type-response">response()</a>], Name::<a href="#type-server_name">server_name()</a>) -&gt; ok
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

<a name="reconnect_state-1"></a>

### reconnect_state/1 ###

<pre><code>
reconnect_state(Options::<a href="#type-client_options">client_options()</a>) -&gt; undefined | <a href="#type-reconnect_state">reconnect_state()</a>
</code></pre>
<br />

<a name="reconnect_state_reset-1"></a>

### reconnect_state_reset/1 ###

<pre><code>
reconnect_state_reset(Reconnect_state::undefined | <a href="#type-reconnect_state">reconnect_state()</a>) -&gt; undefined | <a href="#type-reconnect_state">reconnect_state()</a>
</code></pre>
<br />

<a name="reply-3"></a>

### reply/3 ###

<pre><code>
reply(Name::<a href="#type-server_name">server_name()</a>, Reply::term(), Cast::undefined | <a href="#type-cast">cast()</a>) -&gt; ok
</code></pre>
<br />

<a name="reply_all-2"></a>

### reply_all/2 ###

<pre><code>
reply_all(Name::<a href="#type-server_name">server_name()</a>, Reply::term()) -&gt; ok
</code></pre>
<br />

<a name="warning_msg-3"></a>

### warning_msg/3 ###

<pre><code>
warning_msg(Pool::<a href="#type-pool_name">pool_name()</a>, Format::string(), Data::[term()]) -&gt; ok
</code></pre>
<br />

