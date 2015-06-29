

# Module shackle_queue #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-1">all/1</a></td><td></td></tr><tr><td valign="top"><a href="#in-3">in/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#out-2">out/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-1"></a>

### all/1 ###

<pre><code>
all(ServerName::atom()) -&gt; [term()]
</code></pre>
<br />

<a name="in-3"></a>

### in/3 ###

<pre><code>
in(ServerName::atom(), Stream::non_neg_integer(), Item::term()) -&gt; true
</code></pre>
<br />

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; shackle_queue
</code></pre>
<br />

<a name="out-2"></a>

### out/2 ###

<pre><code>
out(ServerName::atom(), Stream::non_neg_integer()) -&gt; term()
</code></pre>
<br />

