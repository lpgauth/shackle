

# Module shackle_server #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-4">init/4</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#system_code_change-4">system_code_change/4</a></td><td></td></tr><tr><td valign="top"><a href="#system_continue-3">system_continue/3</a></td><td></td></tr><tr><td valign="top"><a href="#system_terminate-4">system_terminate/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-4"></a>

### init/4 ###

<pre><code>
init(Name::atom(), PoolName::atom(), Client::module(), Parent::pid()) -&gt; no_return()
</code></pre>
<br />

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(Name::atom(), PoolName::atom(), Client::module()) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="system_code_change-4"></a>

### system_code_change/4 ###

<pre><code>
system_code_change(State::#state{client = module(), client_state = term(), ip = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>, name = atom(), parent = pid(), pool_name = atom(), port = <a href="inet.md#type-port_number">inet:port_number()</a>, reconnect = boolean(), reconnect_time = non_neg_integer(), socket = undefined | <a href="inet.md#type-socket">inet:socket()</a>, timer = undefined | <a href="timer.md#type-ref">timer:ref()</a>}, Module::module(), OldVsn::undefined | term(), Extra::term()) -&gt; {ok, #state{client = module(), client_state = term(), ip = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>, name = atom(), parent = pid(), pool_name = atom(), port = <a href="inet.md#type-port_number">inet:port_number()</a>, reconnect = boolean(), reconnect_time = non_neg_integer(), socket = undefined | <a href="inet.md#type-socket">inet:socket()</a>, timer = undefined | <a href="timer.md#type-ref">timer:ref()</a>}}
</code></pre>
<br />

<a name="system_continue-3"></a>

### system_continue/3 ###

<pre><code>
system_continue(Parent::pid(), Debug::[], State::#state{client = module(), client_state = term(), ip = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>, name = atom(), parent = pid(), pool_name = atom(), port = <a href="inet.md#type-port_number">inet:port_number()</a>, reconnect = boolean(), reconnect_time = non_neg_integer(), socket = undefined | <a href="inet.md#type-socket">inet:socket()</a>, timer = undefined | <a href="timer.md#type-ref">timer:ref()</a>}) -&gt; ok
</code></pre>
<br />

<a name="system_terminate-4"></a>

### system_terminate/4 ###

<pre><code>
system_terminate(Reason::term(), Parent::pid(), Debug::[], State::#state{client = module(), client_state = term(), ip = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>, name = atom(), parent = pid(), pool_name = atom(), port = <a href="inet.md#type-port_number">inet:port_number()</a>, reconnect = boolean(), reconnect_time = non_neg_integer(), socket = undefined | <a href="inet.md#type-socket">inet:socket()</a>, timer = undefined | <a href="timer.md#type-ref">timer:ref()</a>}) -&gt; none()
</code></pre>
<br />

