

# Module shackle_queue #
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
cast() = #cast{client = <a href="#type-client">client()</a>, pid = undefined | pid(), request_id = <a href="#type-request_id">request_id()</a>, timeout = timeout(), timestamp = <a href="erlang.md#type-timestamp">erlang:timestamp()</a>}
</code></pre>




### <a name="type-client">client()</a> ###


<pre><code>
client() = module()
</code></pre>




### <a name="type-client_option">client_option()</a> ###


<pre><code>
client_option() = {init_options, <a href="#type-init_options">init_options()</a>} | {ip, <a href="#type-inet_address">inet_address()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {protocol, <a href="#type-protocol">protocol()</a>} | {reconnect, boolean()} | {reconnect_time_max, <a href="#type-time">time()</a> | infinity} | {reconnect_time_min, <a href="#type-time">time()</a>} | {socket_options, <a href="#type-socket_options">socket_options()</a>}
</code></pre>




### <a name="type-client_options">client_options()</a> ###


<pre><code>
client_options() = [<a href="#type-client_option">client_option()</a>]
</code></pre>




### <a name="type-external_request_id">external_request_id()</a> ###


<pre><code>
external_request_id() = term()
</code></pre>




### <a name="type-inet_address">inet_address()</a> ###


<pre><code>
inet_address() = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>
</code></pre>




### <a name="type-init_options">init_options()</a> ###


<pre><code>
init_options() = term()
</code></pre>




### <a name="type-max_retries">max_retries()</a> ###


<pre><code>
max_retries() = non_neg_integer()
</code></pre>




### <a name="type-pool_name">pool_name()</a> ###


<pre><code>
pool_name() = atom()
</code></pre>




### <a name="type-pool_option">pool_option()</a> ###


<pre><code>
pool_option() = {backlog_size, <a href="#type-backlog_size">backlog_size()</a>} | {max_retries, <a href="#type-max_retries">max_retries()</a>} | {pool_size, <a href="#type-pool_size">pool_size()</a>} | {pool_strategy, <a href="#type-pool_strategy">pool_strategy()</a>}
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




### <a name="type-request_id">request_id()</a> ###


<pre><code>
request_id() = {<a href="#type-server_name">server_name()</a>, reference()}
</code></pre>




### <a name="type-server_id">server_id()</a> ###


<pre><code>
server_id() = {<a href="#type-pool_name">pool_name()</a>, <a href="#type-server_index">server_index()</a>}
</code></pre>




### <a name="type-server_index">server_index()</a> ###


<pre><code>
server_index() = pos_integer()
</code></pre>




### <a name="type-server_name">server_name()</a> ###


<pre><code>
server_name() = atom()
</code></pre>




### <a name="type-socket_option">socket_option()</a> ###


<pre><code>
socket_option() = <a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a> | <a href="gen_udp.md#type-option">gen_udp:option()</a> | <a href="ssl.md#type-connect_option">ssl:connect_option()</a>
</code></pre>




### <a name="type-socket_options">socket_options()</a> ###


<pre><code>
socket_options() = [<a href="#type-socket_option">socket_option()</a>]
</code></pre>




### <a name="type-time">time()</a> ###


<pre><code>
time() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-4">add/4</a></td><td></td></tr><tr><td valign="top"><a href="#clear-1">clear/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-4"></a>

### add/4 ###

<pre><code>
add(ServerId::<a href="#type-server_id">server_id()</a>, ExtRequestId::<a href="#type-external_request_id">external_request_id()</a>, Cast::<a href="#type-cast">cast()</a>, TimerRef::reference()) -&gt; ok
</code></pre>
<br />

<a name="clear-1"></a>

### clear/1 ###

<pre><code>
clear(ServerId::<a href="#type-server_id">server_id()</a>) -&gt; [{<a href="#type-cast">cast()</a>, reference()}]
</code></pre>
<br />

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; ok
</code></pre>
<br />

<a name="remove-2"></a>

### remove/2 ###

<pre><code>
remove(ServerId::<a href="#type-server_id">server_id()</a>, ExtRequestId::<a href="#type-external_request_id">external_request_id()</a>) -&gt; {ok, <a href="#type-cast">cast()</a>, reference()} | {error, not_found}
</code></pre>
<br />

