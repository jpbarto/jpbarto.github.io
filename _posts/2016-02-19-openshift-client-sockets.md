---
title: OpenShift Client Sockets
layout: single
published: true
categories:
  - Technical
  - Technology
tags:
  - networking
  - openshift
  - python
---
[OpenShift](https://www.openshift.com/ "OpenShift Online") is [Red Hat&#8217;s](http://www.redhat.com "Red Hat") deployment of the Platform as a Service system by the [same name](https://www.openshift.org "OpenShift Origin") and is an incredibly capable environment for hosting custom, rapidly developed, and rapidly deployed services.

Most days clients are connecting IN to the services hosted by OpenShift but occasionally a service owner may want the service to initiate communication with the outside world. For example the service may need to send an email over port 25 to an external mail server, or perhaps make a call out to an external REST service on port 443.
  
Slightly less common examples may include communicating with a remote [RabbitMQ](http://www.rabbitmq.com/ "RabbitMQ") server over port 5672 or with an external [MongoDB](https://www.mongodb.org/ "Mongo DB") server over port 27017.

On the surface an Openshift user may see no need to secure these outgoing transmissions whereas the less trusting user may be able to see why a hosting provider might want to restrict what is and is not permitted.

I found myself in this latter camp and so when I initially received a &#8220;permission denied&#8221; error the first time my code requested an outgoing connection I was not shocked.

<!--more-->

 [Googling](https://www.google.co.uk/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=openshift+outgoing+socket "Google: openshift outgoing socket") in response to the error message revealed that others ([here](http://stackoverflow.com/questions/22057043/openshift-online-outgoing-connections-permission-denied), [here](http://wildfly-development.1055759.n5.nabble.com/java-net-SocketException-Permission-denied-Openshift-td5716537.html) and [here](http://stackoverflow.com/questions/16911325/fail-to-open-an-outgoing-connection-socket-on-openshift)) were receiving similar errors. This [blog post](https://blog.openshift.com/outbound-mail-ports-are-now-open-for-business-on-openshift/ "OpenShift some ports allowed") and [this bugzilla report](https://bugzilla.redhat.com/show_bug.cgi?id=1016805 "OpenShift stance on outgoing sockets") from RedHat seemed to confirm my suspicions regarding their stance on the matter.

Pressing on though with further experimentation (ssh, telnet, wget) revealed that external communication WAS possible which suggested something wrong with my code.

I was using a library package for my specific protocol, as most would, and after digging into its source found that prior to opening a client socket the library was first trying to bind to the local ip. It was this call to bind on the non-localhost ip which was and is a no-no in the OpenShift rule book.

I removed the unnecessary call to bind (client communications will not usually request to be locally connected to a specific ip or port) and was after that off to the races.

To illustrate the following 3 scripts (in Python) attempt to bind to a specific IP, or any IP chosen by the OS, or to implicitly bind and it is only the last script which will be successful:

<pre class="lang:python decode:true" title="Network Client Test Script #1">import socket
import os

my_ip = os.getenv ('OPENSHIFT_PYTHON_IP')

sock = socket.socket (socket.AF_INET, socket.SOCK_STREAM)
sock.bind ((my_ip, 0))
sock.connect (('banks.freenode.net', 8001))
data = sock.recv (128)
print "local socket {0}".format (sock.getsockname ())
print "received from socket: {0}".format(repr (data))
sock.close ()</pre>

<pre class="lang:sh decode:true" title="Test Script #1: Output">Traceback (most recent call last):
File "basic_client_2.py", line 8, in &lt;module&gt;
sock.connect (('banks.freenode.net', 8001))
File "/opt/rh/python27/root/usr/lib64/python2.7/socket.py", line 224, in meth
return getattr(self._sock,name)(*args)
socket.error: [Errno 110] Connection timed out</pre>

Ok binding to the container defined IP didn&#8217;t work, what if we let the OS decide:

<pre class="lang:python decode:true" title="Network Test Script #2">import socket

sock = socket.socket (socket.AF_INET, socket.SOCK_STREAM)
sock.bind (('', 0))
sock.connect (('banks.freenode.net', 8001))
data = sock.recv (128)
print "local socket {0}".format (sock.getsockname ())
print "received from socket: {0}".format(repr (data))
sock.close ()</pre>

<pre class="lang:sh decode:true" title="Test Script #2: Output">Traceback (most recent call last):
File "basic_client_2.py", line 4, in &lt;module&gt;
sock.bind (('', 0))
File "/opt/rh/python27/root/usr/lib64/python2.7/socket.py", line 224, in meth
return getattr(self._sock,name)(*args)
socket.error: [Errno 13] Permission denied</pre>

Ok so that didn&#8217;t work either, what address \*should\* I bind to?

<pre class="lang:default decode:true " title="Network Test Script #3">import socket

sock = socket.socket (socket.AF_INET, socket.SOCK_STREAM)
sock.connect (('banks.freenode.net', 8001))
data = sock.recv (128)
print "local socket {0}".format (sock.getsockname ())
print "received from socket: {0}".format(repr (data))
sock.close ()</pre>

<pre class="lang:default decode:true" title="Test Script #3: Output">local socket ('172.16.6.170', 17919)
received from socket: ':banks.freenode.net NOTICE * :*** Looking up your hostname...\r\n'</pre>

&nbsp;

In Summary&#8230;

Docker, upon which openshift is based, does a lot of port forwarding to map a container&#8217;s ports to actual external ip ports. And OpenShift as a part of its security policy and it&#8217;s multi-tenancy have strict rules and permissions in place on how a service must interact with the ports and IPs provided. They do a good job of documenting these for a user developing and hosting a server. However the documentation to cover external communication, even just something to say it&#8217;s permitted but lookout for any calls to bind in a client situation, is markedly absent. RedHat would do well to include a one-pager in their wiki that captures their policy on client sockets, what ports and protocols are permitted, don&#8217;t use bind, etc. Until then I fear users will continue to be misdirected, frustrated and confused the next time their given a &#8220;permission denied&#8221; when they try to make a rest request to an external service provider.

For more information on port mapping in OpenShift, aside from the official Red Hat OpenShift docs [this link](http://blog-judcon.rhcloud.com/?p=97) is the best I&#8217;ve found to date.
