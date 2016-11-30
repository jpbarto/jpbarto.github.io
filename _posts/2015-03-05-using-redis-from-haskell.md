---
layout: single
status: publish
published: true
title: A Redis API for Haskell
author:
  display_name: Jason Barto
  login: jpbarto
  email: jason.p.barto@gmail.com
  url: ''
author_login: jpbarto
author_email: jason.p.barto@gmail.com
excerpt: "I have been developing a project that uses the <a title=\"Publish and Subscribe\"
  href=\"http:&#47;&#47;www.enterpriseintegrationpatterns.com&#47;PublishSubscribeChannel.html\">publish
  and subscribe<&#47;a> integration pattern with <a title=\"Redis.IO\" href=\"http:&#47;&#47;redis.io&#47;\">Redis<&#47;a>
  acting as the communication broker for messaging. The project started in <a title=\"Python\"
  href=\"https:&#47;&#47;www.python.org&#47;\" target=\"_blank\">Python<&#47;a>, my
  preferred language for rapid prototyping, but because Redis is at the center of
  the system's architecture any software which can talk Redis can communicate with
  the other components. &nbsp;What follows is a discussion of how, from <a title=\"Haskell\"
  href=\"https:&#47;&#47;www.haskell.org&#47;\" target=\"_blank\">Haskell<&#47;a>,
  to communicate with a Redis server.\r\n\r\n"
wordpress_id: 22
wordpress_url: https://www.r9labs.org/?p=22
date: '2015-03-05 22:58:14 -0500'
date_gmt: '2015-03-05 22:58:14 -0500'
categories:
- Languages
- Technical
tags:
- haskell
- redis
comments: []
---
<p>I have been developing a project that uses the <a title="Publish and Subscribe" href="http:&#47;&#47;www.enterpriseintegrationpatterns.com&#47;PublishSubscribeChannel.html">publish and subscribe<&#47;a> integration pattern with <a title="Redis.IO" href="http:&#47;&#47;redis.io&#47;">Redis<&#47;a> acting as the communication broker for messaging. The project started in <a title="Python" href="https:&#47;&#47;www.python.org&#47;" target="_blank">Python<&#47;a>, my preferred language for rapid prototyping, but because Redis is at the center of the system's architecture any software which can talk Redis can communicate with the other components. &nbsp;What follows is a discussion of how, from <a title="Haskell" href="https:&#47;&#47;www.haskell.org&#47;" target="_blank">Haskell<&#47;a>, to communicate with a Redis server.</p>
<p><a id="more"></a><a id="more-22"></a></p>
<p><em>DISCLAIMER: I am a seasoned software developer using Python, Java and C. I have dabbled in Lisp but have only begun to learn Haskell. The code I'm about to discuss works but is likely not best practice, you've been warned.<&#47;em></p>
<p>I've long had an interest, as a professional software developer, in functional programming. In the past I have studied <a title="Common Lisp" href="https:&#47;&#47;common-lisp.net&#47;" target="_blank">Lisp<&#47;a> but I've never implemented any projects using the language. Functional languages like <a title="Scala" href="http:&#47;&#47;www.scala-lang.org&#47;" target="_blank">Scala<&#47;a>, <a title="Clojure" href="http:&#47;&#47;clojure.org&#47;" target="_blank">Clojure<&#47;a> and Haskell now seem to be growing in popularity. In reading up on Haskell, a purely functional language rather than an imperative &#47; functional hybrid, I have found that it compiles down to native binaries (rather than running in a VM like Clojure or <a title="Erlang" href="http:&#47;&#47;www.erlang.org&#47;" target="_blank">Erlang<&#47;a>) and has a large number of software libraries available for use with it, making it potentially very useful for the everyday world. As such I took it upon myself to explore Haskell, implementing a portion of my aforementioned system using the functional language&nbsp;- it will communicate with the other Python components via Redis by passing <a title="JSON" href="http:&#47;&#47;json.org&#47;" target="_blank">Javascript Object Notation<&#47;a>&nbsp;messages.</p>
<p>After reading through some very good and very complete tutorials such as <a title="Learn you a Haskell for Great Good" href="http:&#47;&#47;learnyouahaskell.com&#47;" target="_blank">Learn you a Haskell for Great Good<&#47;a>&nbsp;and <a title="Real World Haskell" href="http:&#47;&#47;book.realworldhaskell.org&#47;" target="_blank">Real World Haskell<&#47;a>&nbsp;the next question was how to actually communicate with Redis using Haskell. &nbsp;The first step was a visit to the very convenient <a title="Haskell Hackage" href="https:&#47;&#47;hackage.haskell.org&#47;" target="_blank">Hackage<&#47;a> website - anyone familiar with <a title="Python PyPI" href="https:&#47;&#47;pypi.python.org&#47;pypi" target="_blank">PyPI<&#47;a> will feel right at home. &nbsp;A quick search of the Hackage website turned up over 10 different packages&nbsp;for communicating with Redis from Haskell. &nbsp;For my purposes, and with little filtering, I chose the top result named simply <em>redis<&#47;em>, only to find that it was deprecated in favor of <em><a title="Haskell hedis Package" href="https:&#47;&#47;hackage.haskell.org&#47;package&#47;hedis" target="_blank">hedis<&#47;a><&#47;em>. &nbsp;Following the documentation for the Database.Redis module (found <a title="Database.Redis Documentation" href="http:&#47;&#47;hackage.haskell.org&#47;package&#47;hedis-0.6.5&#47;docs&#47;Database-Redis.html">here<&#47;a>) I created a sandbox app to exercise the <em>hedis<&#47;em> package. &nbsp;Named '<em>rtalk<&#47;em>' it will establish a connection to the local Redis instance, publish a status&nbsp;message based upon a Redis-stored value and then listen for any messages published to the '<em>rtalk.command<&#47;em>' channel. &nbsp;Upon receipt of a message it will increment a message counter, publish an updated status and go back to waiting. &nbsp;If the message body is '<em>quit<&#47;em>' or '<em>reset<&#47;em>' rtalk will quit running, or reset the message counter and then quit running respectively.</p>
<p><strong>Connection<&#47;strong></p>
<p>To connect was simple enough, using the '<em>connect<&#47;em>' statement and passing it a connection descriptor established a connection which was stored for later use:</p>
<pre class="theme:tomorrow lang:haskell decode:true">conn = connect defaultConnectInfo { connectHost = "localhost", connectPort = (PortNumber 6379) }<&#47;pre><br />
The connection details are nothing outlandish, in fact they are the default values, however other connection parameters include '<em>connectAuth<&#47;em>' which would be a password for connection authorization, '<em>connectDatabase<&#47;em>', '<em>connectMaxConnections<&#47;em>' and '<em>connectMaxIdleTime<&#47;em>' for those with more specific Redis connection needs. As it stands this is test code so the default values worked just fine for my needs.</p>
<p><strong>Set a Value<&#47;strong></p>
<p>Keeping reference to the Redis connection will be needed as it must be passed every time you need to send a command to the Redis server. As an example, to store a value you will need to execute</p>
<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn $ set (pack "key") (pack "value")<&#47;pre><br />
With the above <em>runRedis<&#47;em> will retrieve a connection from the pool of connections referenced by '<em>conn<&#47;em>' and issue the '<em>set<&#47;em>' command. All of the <em>hedis<&#47;em> functions work on <a title="Haskell Data.ByteString" href="https:&#47;&#47;hackage.haskell.org&#47;package&#47;bytestring-0.9.2.1&#47;docs&#47;Data-ByteString.html" target="_blank">ByteString<&#47;a> arguments rather than strings, thus the packing and unpacking. Looking at the examples in the Database.Redis documentation it suggests you can pass bare strings (aka [char]) however this did not work for me, nor is coercion of types an aspect of Haskell, if anyone can tell me if &#47; how something like <span class="theme:tomorrow lang:haskell decode:true crayon-inline">set "key" "value"<&#47;span>&nbsp; would work with a function definition of&nbsp;<span class="theme:tomorrow lang:haskell decode:true crayon-inline">ByteString -> ByteString -> Redis<&#47;span>&nbsp;I would really appreciate the heads up.</p>
<p><span style="color: #0000ff">UPDATE: Shortly after publication, while reading about MySQL interaction I came across the Haskell syntax extension <span class="lang:haskell decode:true  crayon-inline">{-# LANGUAGE OverloadedStrings #-}<&#47;span>&nbsp;&nbsp;which allows for <span class="lang:haskell decode:true  crayon-inline ">[char]<&#47;span>&nbsp;&nbsp;to be morphed to <span class="lang:haskell decode:true  crayon-inline ">ByteString<&#47;span>&nbsp;&nbsp;thus enabling <span class="lang:haskell decode:true  crayon-inline">runRedis conn $ set "key" "value"<&#47;span>.&nbsp;&nbsp; Huzzah!<&#47;span></p>
<p><strong>Set if not Defined<&#47;strong></p>
<p>In the event that you only wish to set a value provided it does not exist, ie set a value without overwriting a value, Redis provides the 'setnx' command.:</p>
<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn $ setnx (pack "key") (pack "value")<&#47;pre><br />
<strong>Get a Value<&#47;strong></p>
<p>Retrieving a value is just as easy:</p>
<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn $ get (pack "key")<&#47;pre><br />
This will return a value of <span class="theme:tomorrow lang:haskell decode:true crayon-inline">Either (Reply (Maybe ByteString))<&#47;span>&nbsp;&nbsp;meaning you'll need to pick out the desired value provided that no errors occurred.</p>
<p><em>hedis<&#47;em> has implemented nearly all of the available Redis commands so you'll likely find all of your needs met.</p>
<p><strong>Increment and Decrement<&#47;strong></p>
<p>Whenever you can push off menial tasks in the guise of atomic functions its always best to do so. Redis recognizes this with commands like incr and decr. &nbsp;To save the user the trouble of retrieving a value, adding 1 to the value and storing it back into the database Redis will increment or decrement the value on your behalf. &nbsp;This is a must if you are working in an environment where multiple agents could potentially be modifying the same value at the same time.</p>
<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn $ incr (pack "key") -- increment the value of 'key' by 1<br />
runRedis conn $ decr (pack "key") -- decrement the value of 'key' by 1<br />
runRedis conn $ incrby (pack "key") 9 -- increment the value of 'key' by 9<br />
runRedis conn $ decrby (pack "key") 3 -- decrement the value of 'key' by 3<&#47;pre><br />
It bears mentioning that when you 'set' or 'get' a value you always send and receive a ByteString. &nbsp;The same is true of numerical values that you intend to later increment and decrement. &nbsp;In other words:</p>
<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn $ do<br />
    set (pack "avar") (pack "13")<br />
    incrby (pack "avar") 10<br />
    get (pack "avar") -- returns 'Right (Just "23")'<&#47;pre><br />
<strong>Publish and Subscribe<&#47;strong></p>
<p>Aside from acting as a key-value store Redis can also act as a messaging broker facilitating publish and subscribe communication. Publishing messages is about as straight forward as setting a key-value:</p>
<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn (publish (pack "channel-name") (pack "message body"))<&#47;pre><br />
Subsciption is equally straight forward:</p>
<pre class="theme:tomorrow lang:haskell decode:true">runRedis conn (pubSub (subscribe [(pack "channel-name")]) handlerFunction)<&#47;pre><br />
A few things worth mentioning: especially for anyone accustomed to working with <a title="Java Messaging Service" href="http:&#47;&#47;en.wikipedia.org&#47;wiki&#47;Java_Message_Service" target="_blank">JMS<&#47;a>, <a title="Advanced Message Queueing Protocol" href="http:&#47;&#47;www.amqp.org&#47;" target="_blank">AMQP<&#47;a> or <a title="Message Queue Telemetry Transport" href="http:&#47;&#47;mqtt.org&#47;" target="_blank">MQTT<&#47;a> in other imperative languages: first in order for the subscription to be maintained <em>hedis<&#47;em> advises that the subscription handler, the message handler, return <em><a title="Data.Monoid mempty" href="https:&#47;&#47;hackage.haskell.org&#47;package&#47;base-4.5.0.0&#47;docs&#47;Data-Monoid.html#v:mempty" target="_blank">mempty<&#47;a><&#47;em>, alternatively to end the subscription the message handler should terminate with:</p>
<pre class="theme:tomorrow lang:haskell decode:true">return (unsubscribe[pack "channel-name"])<&#47;pre><br />
Secondly the call to '<em>pubSub<&#47;em>' will block until the subscription has been cancelled. So if you are creating a multi-threaded asynchronous application I'd suggest spawning each subscription as its own thread and managing those threads using something like the Haskell <a title="Haskell threads" href="https:&#47;&#47;hackage.haskell.org&#47;package&#47;threads" target="_blank">threads<&#47;a> package.</p>
<p>Below you'll find my finished source code for Rtalk, the bit of Redis exercise code. I welcome any comments and suggestions as I'm only learning Haskell at the moment and I'm sure there is a more Haskell-y way of doing things. I hope you've found this helpful and maybe answered a question or two.</p>
<p>Sincerely,<br />
Jason</p>
<pre class="theme:tomorrow lang:haskell decode:true  " title="rtalk.hs">import Control.Concurrent (forkIO)<br />
import Data.ByteString.Char8 (pack, unpack)<br />
import Data.ByteString.Internal (ByteString)<br />
import Data.Monoid (mempty)<br />
import Database.Redis</p>
<p>-- use the pipelining feature of runRedis to<br />
--      retrieve the latest value of rtalk.msg-count<br />
--      if the value is an error (Either Left or Nothing) then publish a '0' message count<br />
--      else publish the value of rtalk.msg-count to rtalk.status<br />
publishStatus conn prefix = runRedis conn $ get (pack "rtalk.msg-count") >>=<br />
        either (\a -> return (pack (prefix ++"0 messages")))<br />
                (\b -> return $ maybe (pack (prefix ++"0 messages"))<br />
                        (\c -> (pack (prefix++ (unpack c) ++" messages"))) b) >>=<br />
        publish (pack "rtalk.status")</p>
<p>reportStatus conn = publishStatus conn "started with "<br />
reportProcessing conn = publishStatus conn "processed "</p>
<p>main = do<br />
        -- create a connection to the local Redis datastore<br />
        conn <- connect defaultConnectInfo { connectHost = "localhost", connectPort = (PortNumber 6379) }<br />
        -- if the value 'rtalk.msg-count' does not exist, set it to an initial value of '0'<br />
        runRedis conn $ setnx (pack "rtalk.msg-count") (pack "0")<br />
        -- publish out an initial 'im alive' status message<br />
        reportStatus conn<br />
        -- listen for any messages published to rtalk.command<br />
        runRedis conn $ pubSub (subscribe [(pack "rtalk.command")]) $ \message -> do<br />
                -- write to the console our receipt of a message<br />
                putStrLn ("Received: "++ (show (msgMessage message)))<br />
                -- increment the number of messages received<br />
                runRedis conn $ incr (pack "rtalk.msg-count")<br />
                -- publish an updated message count status<br />
                reportProcessing conn<br />
                -- if the message received contained a command respond accordingly<br />
                case (unpack (msgMessage message)) of<br />
                        -- if the message was to 'quit' then unsubscribe and exit<br />
                        "quit" -> return $ unsubscribe[(pack "rtalk.command")]<br />
                        -- if the message was to 'reset' then clear the message count, unsubscribe and exit<br />
                        "reset" -> do runRedis conn $ del [(pack "rtalk.msg-count")]; return $ unsubscribe[(pack "rtalk.command")]<br />
                        -- otherwise carry on listening and counting<br />
                        _ -> return mempty<br />
<&#47;pre></p>
