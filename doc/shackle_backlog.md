

# Module shackle_backlog #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check-1">check/1</a></td><td></td></tr><tr><td valign="top"><a href="#decrement-1">decrement/1</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="check-1"></a>

### check/1 ###

<pre><code>
check(ServerName::atom()) -&gt; boolean()
</code></pre>
<br />

<a name="decrement-1"></a>

### decrement/1 ###

<pre><code>
decrement(ServerName::atom()) -&gt; non_neg_integer() | {error, tid_missing}
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
new(ServerName::atom()) -&gt; true
</code></pre>
<br />

