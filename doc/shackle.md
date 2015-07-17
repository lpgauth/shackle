

# Module shackle #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-2">call/2</a></td><td></td></tr><tr><td valign="top"><a href="#call-3">call/3</a></td><td></td></tr><tr><td valign="top"><a href="#cast-3">cast/3</a></td><td></td></tr><tr><td valign="top"><a href="#receive_response-3">receive_response/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-2"></a>

### call/2 ###

<pre><code>
call(Name::atom(), Msg::term()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="call-3"></a>

### call/3 ###

<pre><code>
call(Name::atom(), Msg::term(), Timeout::pos_integer()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="cast-3"></a>

### cast/3 ###

<pre><code>
cast(Name::atom(), Msg::term(), Pid::pid()) -&gt; {ok, reference()} | {error, backlog_full}
</code></pre>
<br />

<a name="receive_response-3"></a>

### receive_response/3 ###

<pre><code>
receive_response(Name::module(), Ref::reference(), Timeout::pos_integer()) -&gt; {ok, reference()} | {error, term()}
</code></pre>
<br />

