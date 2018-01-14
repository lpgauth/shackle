

# Module shackle_tcp_server #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-backlog_size">backlog_size()</a> ###


<pre><code>
backlog_size() = pos_integer() | infinity
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




### <a name="type-init_opts">init_opts()</a> ###


<pre><code>
init_opts() = {<a href="#type-pool_name">pool_name()</a>, <a href="#type-client">client()</a>, <a href="#type-client_options">client_options()</a>}
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




### <a name="type-server_name">server_name()</a> ###


<pre><code>
server_name() = atom()
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = #state{client = <a href="#type-client">client()</a>, ip = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>, name = <a href="#type-server_name">server_name()</a>, parent = pid(), pool_name = <a href="#type-pool_name">pool_name()</a>, port = <a href="inet.md#type-port_number">inet:port_number()</a>, reconnect_state = undefined | <a href="#type-reconnect_state">reconnect_state()</a>, socket = undefined | <a href="inet.md#type-socket">inet:socket()</a>, socket_options = [<a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a>], timer_ref = undefined | reference()}
</code></pre>




### <a name="type-time">time()</a> ###


<pre><code>
time() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle_msg-2">handle_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="handle_msg-2"></a>

### handle_msg/2 ###

<pre><code>
handle_msg(Cast::term(), X2::{<a href="#type-state">state()</a>, <a href="#type-client_state">client_state()</a>}) -&gt; {ok, term()}
</code></pre>
<br />

<a name="init-3"></a>

### init/3 ###

<pre><code>
init(Name::<a href="#type-server_name">server_name()</a>, Parent::pid(), Opts::<a href="#type-init_opts">init_opts()</a>) -&gt; no_return()
</code></pre>
<br />

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(Name::<a href="#type-server_name">server_name()</a>, PoolName::<a href="#type-pool_name">pool_name()</a>, Client::<a href="#type-client">client()</a>, ClientOptions::<a href="#type-client_options">client_options()</a>) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

<pre><code>
terminate(Reason::term(), X2::term()) -&gt; ok
</code></pre>
<br />

