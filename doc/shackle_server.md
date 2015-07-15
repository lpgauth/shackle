

# Module shackle_server #
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `shackle_server` behaviour.__<br /> Required callback functions: `init/0`, `after_connect/2`, `handle_cast/2`, `handle_data/2`, `terminate/1`.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td></td></tr><tr><td valign="top"><a href="#system_code_change-4">system_code_change/4</a></td><td></td></tr><tr><td valign="top"><a href="#system_continue-3">system_continue/3</a></td><td></td></tr><tr><td valign="top"><a href="#system_terminate-4">system_terminate/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-3"></a>

### init/3 ###

<pre><code>
init(Name::atom(), Module::module(), Parent::pid()) -&gt; no_return()
</code></pre>
<br />

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Name::atom(), Module::module()) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="system_code_change-4"></a>

### system_code_change/4 ###

<pre><code>
system_code_change(State::#state{connect_retry = non_neg_integer(), ip = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>, module = module(), name = atom(), parent = pid(), port = <a href="inet.md#type-port_number">inet:port_number()</a>, reconnect = boolean(), socket = undefined | <a href="inet.md#type-socket">inet:socket()</a>, state = term(), timer = undefined | <a href="timer.md#type-ref">timer:ref()</a>}, Module::module(), OldVsn::undefined | term(), Extra::term()) -&gt; {ok, #state{connect_retry = non_neg_integer(), ip = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>, module = module(), name = atom(), parent = pid(), port = <a href="inet.md#type-port_number">inet:port_number()</a>, reconnect = boolean(), socket = undefined | <a href="inet.md#type-socket">inet:socket()</a>, state = term(), timer = undefined | <a href="timer.md#type-ref">timer:ref()</a>}}
</code></pre>
<br />

<a name="system_continue-3"></a>

### system_continue/3 ###

<pre><code>
system_continue(Parent::pid(), Debug::[], State::#state{connect_retry = non_neg_integer(), ip = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>, module = module(), name = atom(), parent = pid(), port = <a href="inet.md#type-port_number">inet:port_number()</a>, reconnect = boolean(), socket = undefined | <a href="inet.md#type-socket">inet:socket()</a>, state = term(), timer = undefined | <a href="timer.md#type-ref">timer:ref()</a>}) -&gt; ok
</code></pre>
<br />

<a name="system_terminate-4"></a>

### system_terminate/4 ###

<pre><code>
system_terminate(Reason::term(), Parent::pid(), Debug::[], State::#state{connect_retry = non_neg_integer(), ip = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>, module = module(), name = atom(), parent = pid(), port = <a href="inet.md#type-port_number">inet:port_number()</a>, reconnect = boolean(), socket = undefined | <a href="inet.md#type-socket">inet:socket()</a>, state = term(), timer = undefined | <a href="timer.md#type-ref">timer:ref()</a>}) -&gt; none()
</code></pre>
<br />

