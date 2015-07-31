

# Module shackle #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-backlog_size">backlog_size()</a> ###


<pre><code>
backlog_size() = pos_integer()
</code></pre>




### <a name="type-client">client()</a> ###


<pre><code>
client() = module()
</code></pre>




### <a name="type-client_option">client_option()</a> ###


<pre><code>
client_option() = {connect_options, [<a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a>]} | {ip, <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {reconnect, boolean()} | {reconnect_time_max, <a href="#type-time">time()</a>} | {reconnect_time_min, <a href="#type-time">time()</a>} | {state, term()}
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




### <a name="type-shackle_req_id">shackle_req_id()</a> ###


<pre><code>
shackle_req_id() = {<a href="#type-pool_name">pool_name()</a>, <a href="#type-client">client()</a>, reference()}
</code></pre>




### <a name="type-time">time()</a> ###


<pre><code>
time() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-2">call/2</a></td><td></td></tr><tr><td valign="top"><a href="#call-3">call/3</a></td><td></td></tr><tr><td valign="top"><a href="#cast-3">cast/3</a></td><td></td></tr><tr><td valign="top"><a href="#receive_response-2">receive_response/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-2"></a>

### call/2 ###

<pre><code>
call(PoolName::<a href="#type-pool_name">pool_name()</a>, Call::term()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="call-3"></a>

### call/3 ###

<pre><code>
call(PoolName::atom(), Call::term(), Timeout::timeout()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="cast-3"></a>

### cast/3 ###

<pre><code>
cast(PoolName::<a href="#type-pool_name">pool_name()</a>, Cast::term(), Pid::pid()) -&gt; {ok, <a href="#type-shackle_req_id">shackle_req_id()</a>} | {error, backlog_full}
</code></pre>
<br />

<a name="receive_response-2"></a>

### receive_response/2 ###

<pre><code>
receive_response(RequestId::<a href="#type-shackle_req_id">shackle_req_id()</a>, Timeout::timeout()) -&gt; {ok, reference()} | {error, term()}
</code></pre>
<br />

