

# Module shackle_udp #
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




### <a name="type-time">time()</a> ###


<pre><code>
time() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#header-2">header/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#send-3">send/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Socket::<a href="inet.md#type-socket">inet:socket()</a>) -&gt; ok
</code></pre>
<br />

<a name="header-2"></a>

### header/2 ###

<pre><code>
header(X1::<a href="inet.md#type-ip_address">inet:ip_address()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; iodata()
</code></pre>
<br />

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Ip::<a href="inet.md#type-ip_address">inet:ip_address()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>, Options::[<a href="gen_udp.md#type-connect_option">gen_udp:connect_option()</a>]) -&gt; {ok, <a href="inet.md#type-socket">inet:socket()</a>} | {error, term()}
</code></pre>
<br />

<a name="send-3"></a>

### send/3 ###

<pre><code>
send(Socket::<a href="inet.md#type-socket">inet:socket()</a>, Header::iodata(), Data::iodata()) -&gt; ok | {error, term()}
</code></pre>
<br />

