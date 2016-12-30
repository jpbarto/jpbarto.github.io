---
title: A Redis API for Haskell
layout: single
published: true
categories:
  - Languages
  - Technical
tags:
  - haskell
  - redis
---
I have been developing a project that uses the [publish and subscribe](http://www.enterpriseintegrationpatterns.com/PublishSubscribeChannel.html "Publish and Subscribe") integration pattern with [Redis](http://redis.io/ "Redis.IO") acting as the communication broker for messaging. The project started in <a title="Python" href="https://www.python.org/" target="_blank">Python</a>, my preferred language for rapid prototyping, but because Redis is at the center of the system&#8217;s architecture any software which can talk Redis can communicate with the other components.  What follows is a discussion of how, from <a title="Haskell" href="https://www.haskell.org/" target="_blank">Haskell</a>, to communicate with a Redis server.

<!--more-->

_DISCLAIMER: I am a seasoned software developer using Python, Java and C. I have dabbled in Lisp but have only begun to learn Haskell. The code I&#8217;m about to discuss works but is likely not best practice, you&#8217;ve been warned._

I&#8217;ve long had an interest, as a professional software developer, in functional programming. In the past I have studied <a title="Common Lisp" href="https://common-lisp.net/" target="_blank">Lisp</a> but I&#8217;ve never implemented any projects using the language. Functional languages like <a title="Scala" href="http://www.scala-lang.org/" target="_blank">Scala</a>, <a title="Clojure" href="http://clojure.org/" target="_blank">Clojure</a> and Haskell now seem to be growing in popularity. In reading up on Haskell, a purely functional language rather than an imperative / functional hybrid, I have found that it compiles down to native binaries (rather than running in a VM like Clojure or <a title="Erlang" href="http://www.erlang.org/" target="_blank">Erlang</a>) and has a large number of software libraries available for use with it, making it potentially very useful for the everyday world. As such I took it upon myself to explore Haskell, implementing a portion of my aforementioned system using the functional language &#8211; it will communicate with the other Python components via Redis by passing <a title="JSON" href="http://json.org/" target="_blank">Javascript Object Notation</a> messages.

After reading through some very good and very complete tutorials such as <a title="Learn you a Haskell for Great Good" href="http://learnyouahaskell.com/" target="_blank">Learn you a Haskell for Great Good</a> and <a title="Real World Haskell" href="http://book.realworldhaskell.org/" target="_blank">Real World Haskell</a> the next question was how to actually communicate with Redis using Haskell.  The first step was a visit to the very convenient <a title="Haskell Hackage" href="https://hackage.haskell.org/" target="_blank">Hackage</a> website &#8211; anyone familiar with <a title="Python PyPI" href="https://pypi.python.org/pypi" target="_blank">PyPI</a> will feel right at home.  A quick search of the Hackage website turned up over 10 different packages for communicating with Redis from Haskell.  For my purposes, and with little filtering, I chose the top result named simply _redis_, only to find that it was deprecated in favor of _<a title="Haskell hedis Package" href="https://hackage.haskell.org/package/hedis" target="_blank">hedis</a>_.  Following the documentation for the Database.Redis module (found [here](http://hackage.haskell.org/package/hedis-0.6.5/docs/Database-Redis.html "Database.Redis Documentation")) I created a sandbox app to exercise the _hedis_ package.  Named &#8216;_rtalk_&#8216; it will establish a connection to the local Redis instance, publish a status message based upon a Redis-stored value and then listen for any messages published to the &#8216;_rtalk.command_&#8216; channel.  Upon receipt of a message it will increment a message counter, publish an updated status and go back to waiting.  If the message body is &#8216;_quit_&#8216; or &#8216;_reset_&#8216; rtalk will quit running, or reset the message counter and then quit running respectively.

**Connection**

To connect was simple enough, using the &#8216;_connect_&#8216; statement and passing it a connection descriptor established a connection which was stored for later use:

<pre class="theme:tomorrow lang:haskell decode:true">conn = connect defaultConnectInfo { connectHost = "localhost", connectPort = (PortNumber 6379) }</pre>

The connection details are nothing outlandish, in fact they are the default values, however other connection parameters include &#8216;_connectAuth_&#8216; which would be a password for connection authorization, &#8216;_connectDatabase_&#8216;, &#8216;_connectMaxConnections_&#8216; and &#8216;_connectMaxIdleTime_&#8216; for those with more specific Redis connection needs. As it stands this is test code so the default values worked just fine for my needs.

**Set a Value**

Keeping reference to the Redis connection will be needed as it must be passed every time you need to send a command to the Redis server. As an example, to store a value you will need to execute

<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn $ set (pack "key") (pack "value")</pre>

With the above _runRedis_ will retrieve a connection from the pool of connections referenced by &#8216;_conn_&#8216; and issue the &#8216;_set_&#8216; command. All of the _hedis_ functions work on <a title="Haskell Data.ByteString" href="https://hackage.haskell.org/package/bytestring-0.9.2.1/docs/Data-ByteString.html" target="_blank">ByteString</a> arguments rather than strings, thus the packing and unpacking. Looking at the examples in the Database.Redis documentation it suggests you can pass bare strings (aka [char]) however this did not work for me, nor is coercion of types an aspect of Haskell, if anyone can tell me if / how something like <span class="theme:tomorrow lang:haskell decode:true crayon-inline">set &#8220;key&#8221; &#8220;value&#8221;</span>  would work with a function definition of <span class="theme:tomorrow lang:haskell decode:true crayon-inline">ByteString -> ByteString -> Redis</span> I would really appreciate the heads up.

<span style="color: #0000ff">UPDATE: Shortly after publication, while reading about MySQL interaction I came across the Haskell syntax extension <span class="lang:haskell decode:true  crayon-inline">{-# LANGUAGE OverloadedStrings #-}</span>  which allows for <span class="lang:haskell decode:true  crayon-inline ">[char]</span>  to be morphed to <span class="lang:haskell decode:true  crayon-inline ">ByteString</span>  thus enabling <span class="lang:haskell decode:true  crayon-inline">runRedis conn $ set &#8220;key&#8221; &#8220;value&#8221;</span>.   Huzzah!</span>

**Set if not Defined**

In the event that you only wish to set a value provided it does not exist, ie set a value without overwriting a value, Redis provides the &#8216;setnx&#8217; command.:

<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn $ setnx (pack "key") (pack "value")</pre>

**Get a Value**

Retrieving a value is just as easy:

<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn $ get (pack "key")</pre>

This will return a value of <span class="theme:tomorrow lang:haskell decode:true crayon-inline">Either (Reply (Maybe ByteString))</span>  meaning you&#8217;ll need to pick out the desired value provided that no errors occurred.

_hedis_ has implemented nearly all of the available Redis commands so you&#8217;ll likely find all of your needs met.

**Increment and Decrement**

Whenever you can push off menial tasks in the guise of atomic functions its always best to do so. Redis recognizes this with commands like incr and decr.  To save the user the trouble of retrieving a value, adding 1 to the value and storing it back into the database Redis will increment or decrement the value on your behalf.  This is a must if you are working in an environment where multiple agents could potentially be modifying the same value at the same time.

<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn $ incr (pack "key") -- increment the value of 'key' by 1
runRedis conn $ decr (pack "key") -- decrement the value of 'key' by 1
runRedis conn $ incrby (pack "key") 9 -- increment the value of 'key' by 9
runRedis conn $ decrby (pack "key") 3 -- decrement the value of 'key' by 3</pre>

It bears mentioning that when you &#8216;set&#8217; or &#8216;get&#8217; a value you always send and receive a ByteString.  The same is true of numerical values that you intend to later increment and decrement.  In other words:

<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn $ do
    set (pack "avar") (pack "13")
    incrby (pack "avar") 10
    get (pack "avar") -- returns 'Right (Just "23")'</pre>

**Publish and Subscribe**

Aside from acting as a key-value store Redis can also act as a messaging broker facilitating publish and subscribe communication. Publishing messages is about as straight forward as setting a key-value:

<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn (publish (pack "channel-name") (pack "message body"))</pre>

Subsciption is equally straight forward:

<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn (pubSub (subscribe [(pack "channel-name")]) handlerFunction)</pre>

A few things worth mentioning: especially for anyone accustomed to working with <a title="Java Messaging Service" href="http://en.wikipedia.org/wiki/Java_Message_Service" target="_blank">JMS</a>, <a title="Advanced Message Queueing Protocol" href="http://www.amqp.org/" target="_blank">AMQP</a> or <a title="Message Queue Telemetry Transport" href="http://mqtt.org/" target="_blank">MQTT</a> in other imperative languages: first in order for the subscription to be maintained _hedis_ advises that the subscription handler, the message handler, return _<a title="Data.Monoid mempty" href="https://hackage.haskell.org/package/base-4.5.0.0/docs/Data-Monoid.html#v:mempty" target="_blank">mempty</a>_, alternatively to end the subscription the message handler should terminate with:

<pre class="theme:tomorrow lang:haskell decode:true">return (unsubscribe[pack "channel-name"])</pre>

Secondly the call to &#8216;_pubSub_&#8216; will block until the subscription has been cancelled. So if you are creating a multi-threaded asynchronous application I&#8217;d suggest spawning each subscription as its own thread and managing those threads using something like the Haskell <a title="Haskell threads" href="https://hackage.haskell.org/package/threads" target="_blank">threads</a> package.

Below you&#8217;ll find my finished source code for Rtalk, the bit of Redis exercise code. I welcome any comments and suggestions as I&#8217;m only learning Haskell at the moment and I&#8217;m sure there is a more Haskell-y way of doing things. I hope you&#8217;ve found this helpful and maybe answered a question or two.

Sincerely,
  
Jason

<pre class="theme:tomorrow lang:haskell decode:true  " title="rtalk.hs">import Control.Concurrent (forkIO)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Internal (ByteString)
import Data.Monoid (mempty)
import Database.Redis

-- use the pipelining feature of runRedis to
--      retrieve the latest value of rtalk.msg-count
--      if the value is an error (Either Left or Nothing) then publish a '0' message count
--      else publish the value of rtalk.msg-count to rtalk.status
publishStatus conn prefix = runRedis conn $ get (pack "rtalk.msg-count") &gt;&gt;=
        either (\a -&gt; return (pack (prefix ++"0 messages")))
                (\b -&gt; return $ maybe (pack (prefix ++"0 messages"))
                        (\c -&gt; (pack (prefix++ (unpack c) ++" messages"))) b) &gt;&gt;=
        publish (pack "rtalk.status")

reportStatus conn = publishStatus conn "started with "
reportProcessing conn = publishStatus conn "processed "

main = do
        -- create a connection to the local Redis datastore
        conn &lt;- connect defaultConnectInfo { connectHost = "localhost", connectPort = (PortNumber 6379) }
        -- if the value 'rtalk.msg-count' does not exist, set it to an initial value of '0'
        runRedis conn $ setnx (pack "rtalk.msg-count") (pack "0")
        -- publish out an initial 'im alive' status message
        reportStatus conn
        -- listen for any messages published to rtalk.command
        runRedis conn $ pubSub (subscribe [(pack "rtalk.command")]) $ \message -&gt; do
                -- write to the console our receipt of a message
                putStrLn ("Received: "++ (show (msgMessage message)))
                -- increment the number of messages received
                runRedis conn $ incr (pack "rtalk.msg-count")
                -- publish an updated message count status
                reportProcessing conn
                -- if the message received contained a command respond accordingly
                case (unpack (msgMessage message)) of
                        -- if the message was to 'quit' then unsubscribe and exit
                        "quit" -&gt; return $ unsubscribe[(pack "rtalk.command")]
                        -- if the message was to 'reset' then clear the message count, unsubscribe and exit
                        "reset" -&gt; do runRedis conn $ del [(pack "rtalk.msg-count")]; return $ unsubscribe[(pack "rtalk.command")]
                        -- otherwise carry on listening and counting
                        _ -&gt; return mempty
</pre>
