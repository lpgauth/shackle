

# Module shackle_backlog #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-backlog_size">backlog_size()</a> ###


<pre><code>
backlog_size() = pos_integer()
</code></pre>




### <a name="type-client_option">client_option()</a> ###


<pre><code>
client_option() = {ip, <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {protocol, <a href="#type-protocol">protocol()</a>} | {reconnect, boolean()} | {reconnect_time_max, <a href="#type-time">time()</a>} | {reconnect_time_min, <a href="#type-time">time()</a>} | {socket_options, [<a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a> | <a href="gen_udp.md#type-option">gen_udp:option()</a>]}
</code></pre>




### <a name="type-client_options">client_options()</a> ###


<pre><code>
client_options() = [<a href="#type-client_option">client_option()</a>]
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




### <a name="type-request_id">request_id()</a> ###


<pre><code>
request_id() = {<a href="#type-server_name">server_name()</a>, reference()}
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check-2">check/2</a></td><td></td></tr><tr><td valign="top"><a href="#decrement-1">decrement/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="check-2"></a>

### check/2 ###

<pre><code>
check(ServerName::<a href="#type-server_name">server_name()</a>, BacklogSize::<a href="#type-backlog_size">backlog_size()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="decrement-1"></a>

### decrement/1 ###

<pre><code>
decrement(ServerName::<a href="#type-server_name">server_name()</a> | <a href="#type-request_id">request_id()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(ServerName::<a href="#type-server_name">server_name()</a>) -&gt; ok
</code></pre>
<br />

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; shackle_backlog
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(ServerName::<a href="#type-server_name">server_name()</a>) -&gt; ok
</code></pre>
<br />

