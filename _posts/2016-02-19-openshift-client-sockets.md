---
layout: single
status: publish
published: true
title: OpenShift Client Sockets
author:
  display_name: Jason Barto
  login: jpbarto
  email: jason.p.barto@gmail.com
  url: ''
author_login: jpbarto
author_email: jason.p.barto@gmail.com
excerpt: "<a title=\"OpenShift Online\" href=\"https:&#47;&#47;www.openshift.com&#47;\">OpenShift<&#47;a>
  is <a title=\"Red Hat\" href=\"http:&#47;&#47;www.redhat.com\">Red Hat's<&#47;a>
  deployment of the Platform as a Service system by the <a title=\"OpenShift Origin\"
  href=\"https:&#47;&#47;www.openshift.org\">same name<&#47;a> and is an incredibly
  capable environment for hosting custom, rapidly developed, and rapidly deployed
  services.\r\n\r\nMost days clients are connecting IN to the services hosted by OpenShift
  but occasionally a service owner may want the service to initiate communication
  with the outside world. For example the service may need to send an email&nbsp;over
  port 25&nbsp;to an external mail server, or perhaps make&nbsp;a call out to an external
  REST service on port 443.\r\nSlightly less common examples may include communicating
  with a remote <a title=\"RabbitMQ\" href=\"http:&#47;&#47;www.rabbitmq.com&#47;\">RabbitMQ<&#47;a>
  server over port 5672 or with an external <a title=\"Mongo DB\" href=\"https:&#47;&#47;www.mongodb.org&#47;\">MongoDB<&#47;a>&nbsp;server
  over port 27017.\r\n\r\nOn the surface an Openshift user may see no need to secure
  these outgoing transmissions whereas the less trusting user may be able to see why
  a hosting provider might want to restrict what is and is not permitted.\r\n\r\nI
  found myself in this latter camp and so when I initially received a \"permission
  denied\" error the first time my code requested an outgoing connection I was not
  shocked.\r\n\r\n"
wordpress_id: 107
wordpress_url: https://www.r9labs.org/?p=107
date: '2016-02-19 14:11:23 -0500'
date_gmt: '2016-02-19 14:11:23 -0500'
categories:
- Technology
- Technical
tags:
- openshift
- networking
- python
comments: []
---
<p><a title="OpenShift Online" href="https:&#47;&#47;www.openshift.com&#47;">OpenShift<&#47;a> is <a title="Red Hat" href="http:&#47;&#47;www.redhat.com">Red Hat's<&#47;a> deployment of the Platform as a Service system by the <a title="OpenShift Origin" href="https:&#47;&#47;www.openshift.org">same name<&#47;a> and is an incredibly capable environment for hosting custom, rapidly developed, and rapidly deployed services.</p>
<p>Most days clients are connecting IN to the services hosted by OpenShift but occasionally a service owner may want the service to initiate communication with the outside world. For example the service may need to send an email&nbsp;over port 25&nbsp;to an external mail server, or perhaps make&nbsp;a call out to an external REST service on port 443.<br />
Slightly less common examples may include communicating with a remote <a title="RabbitMQ" href="http:&#47;&#47;www.rabbitmq.com&#47;">RabbitMQ<&#47;a> server over port 5672 or with an external <a title="Mongo DB" href="https:&#47;&#47;www.mongodb.org&#47;">MongoDB<&#47;a>&nbsp;server over port 27017.</p>
<p>On the surface an Openshift user may see no need to secure these outgoing transmissions whereas the less trusting user may be able to see why a hosting provider might want to restrict what is and is not permitted.</p>
<p>I found myself in this latter camp and so when I initially received a "permission denied" error the first time my code requested an outgoing connection I was not shocked.</p>
<p><a id="more"></a><a id="more-107"></a></p>
<p><a title="Google: openshift outgoing socket" href="https:&#47;&#47;www.google.co.uk&#47;webhp?sourceid=chrome-instant&amp;ion=1&amp;espv=2&amp;ie=UTF-8#q=openshift+outgoing+socket"> Googling<&#47;a> in response to the error message revealed that others (<a href="http:&#47;&#47;stackoverflow.com&#47;questions&#47;22057043&#47;openshift-online-outgoing-connections-permission-denied">here<&#47;a>, <a href="http:&#47;&#47;wildfly-development.1055759.n5.nabble.com&#47;java-net-SocketException-Permission-denied-Openshift-td5716537.html">here<&#47;a> and <a href="http:&#47;&#47;stackoverflow.com&#47;questions&#47;16911325&#47;fail-to-open-an-outgoing-connection-socket-on-openshift">here<&#47;a>) were receiving similar errors. This <a title="OpenShift some ports allowed" href="https:&#47;&#47;blog.openshift.com&#47;outbound-mail-ports-are-now-open-for-business-on-openshift&#47;">blog post<&#47;a> and <a title="OpenShift stance on outgoing sockets" href="https:&#47;&#47;bugzilla.redhat.com&#47;show_bug.cgi?id=1016805">this bugzilla report<&#47;a>&nbsp;from RedHat seemed to confirm my suspicions regarding their stance on the matter.</p>
<p>Pressing on though with further experimentation (ssh, telnet, wget) revealed that external communication WAS possible which suggested something wrong with my code.</p>
<p>I was using a library package for my specific protocol, as most would, and after digging into its source found that prior to opening a client socket the library was first trying to bind to the local ip. It was this call to bind on the non-localhost ip which was and is a no-no in the OpenShift rule book.</p>
<p>I removed the unnecessary call to bind (client communications will not usually request to be locally connected to a specific ip or port) and was after that off to the races.</p>
<p>To illustrate the following 3 scripts (in Python) attempt to bind to a specific IP, or any IP chosen by the OS, or to implicitly bind and it is only the last script which will be successful:</p>
<pre class="lang:python decode:true" title="Network Client Test Script #1">import socket<br />
import os</p>
<p>my_ip = os.getenv ('OPENSHIFT_PYTHON_IP')</p>
<p>sock = socket.socket (socket.AF_INET, socket.SOCK_STREAM)<br />
sock.bind ((my_ip, 0))<br />
sock.connect (('banks.freenode.net', 8001))<br />
data = sock.recv (128)<br />
print "local socket {0}".format (sock.getsockname ())<br />
print "received from socket: {0}".format(repr (data))<br />
sock.close ()<&#47;pre></p>
<pre class="lang:sh decode:true" title="Test Script #1: Output">Traceback (most recent call last):<br />
File "basic_client_2.py", line 8, in <module><br />
sock.connect (('banks.freenode.net', 8001))<br />
File "&#47;opt&#47;rh&#47;python27&#47;root&#47;usr&#47;lib64&#47;python2.7&#47;socket.py", line 224, in meth<br />
return getattr(self._sock,name)(*args)<br />
socket.error: [Errno 110] Connection timed out<&#47;pre><br />
Ok binding to the container defined IP didn't work, what if we let the OS decide:</p>
<pre class="lang:python decode:true" title="Network Test Script #2">import socket</p>
<p>sock = socket.socket (socket.AF_INET, socket.SOCK_STREAM)<br />
sock.bind (('', 0))<br />
sock.connect (('banks.freenode.net', 8001))<br />
data = sock.recv (128)<br />
print "local socket {0}".format (sock.getsockname ())<br />
print "received from socket: {0}".format(repr (data))<br />
sock.close ()<&#47;pre></p>
<pre class="lang:sh decode:true" title="Test Script #2: Output">Traceback (most recent call last):<br />
File "basic_client_2.py", line 4, in <module><br />
sock.bind (('', 0))<br />
File "&#47;opt&#47;rh&#47;python27&#47;root&#47;usr&#47;lib64&#47;python2.7&#47;socket.py", line 224, in meth<br />
return getattr(self._sock,name)(*args)<br />
socket.error: [Errno 13] Permission denied<&#47;pre><br />
Ok so that didn't work either, what address *should* I bind to?</p>
<pre class="lang:default decode:true " title="Network Test Script #3">import socket</p>
<p>sock = socket.socket (socket.AF_INET, socket.SOCK_STREAM)<br />
sock.connect (('banks.freenode.net', 8001))<br />
data = sock.recv (128)<br />
print "local socket {0}".format (sock.getsockname ())<br />
print "received from socket: {0}".format(repr (data))<br />
sock.close ()<&#47;pre></p>
<pre class="lang:default decode:true" title="Test Script #3: Output">local socket ('172.16.6.170', 17919)<br />
received from socket: ':banks.freenode.net NOTICE * :*** Looking up your hostname...\r\n'<&#47;pre><br />
&nbsp;</p>
<p>In Summary...</p>
<p>Docker, upon which openshift is based, does a lot of port forwarding to map a container's ports [OpenShift] is [Red Hat’s] deployment of the Platform as a Service system by the [same name] and is an incredibly capable environment for hosting custom, rapidly developed, and rapidly deployed services. Most days clients are connecting IN to the services hosted by OpenShift but occasionally a service owner may want the service to initiate communication with the outside world. For example the service may need to send an email over port 25 to an external mail server, or perhaps make a call out to an external REST service on port 443. Slightly less common examples may include communicating with a remote [RabbitMQ] server over port 5672 or with an external [MongoDB] server over port 27017. On the surface an Openshift user may see no need to secure these outgoing transmissions whereas the less trusting user may be able to see why a hosting provider might want to restrict what is and is not permitted. I found myself in this latter camp and so when I initially received a “permission denied” error the first time my code requested an outgoing connection I was not shocked. <!--more--> [Googling] in response to the error message revealed that others ([here], [here][1] and [here][2]) were receiving similar errors. This [blog post] and [this bugzilla report] from RedHat seemed to confirm my suspicions regarding their stance on the matter. Pressing on though with further experimentation (ssh, telnet, wget) revealed that external communication WAS possible which suggested something wrong with my code. I was using a library package for my specific protocol, as most would, and after digging into its source found that prior to opening a client socket the library was first trying to bind to the local ip. It was this call to bind on the non-localhost ip which was and is a no-no in the OpenShift rule book. I removed the unnecessary call to bind (client communications will not usually requ

  [OpenShift]: https://www.openshift.com/ "OpenShift Online"
  [Red Hat’s]: http://www.redhat.com "Red Hat"
  [same name]: https://www.openshift.org "OpenShift Origin"
  [RabbitMQ]: http://www.rabbitmq.com/ "RabbitMQ"
  [MongoDB]: https://www.mongodb.org/ "Mongo DB"
  [Googling]: https://www.google.co.uk/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=openshift+outgoing+socket "Google: openshift outgoing socket"
  [here]: http://stackoverflow.com/questions/22057043/openshift-online-outgoing-connections-permission-denied
  [1]: http://wildfly-development.1055759.n5.nabble.com/java-net-SocketException-Permission-denied-Openshift-td5716537.html
  [2]: http://stackoverflow.com/questions/16911325/fail-to-open-an-outgoing-connection-socket-on-openshift
  [blog post]: https://blog.openshift.com/outbound-mail-ports-are-now-open-for-business-on-openshift/ "OpenShift some ports allowed"
  [this bugzilla report]: https://bugzilla.redhat.com/show_bug.cgi?id=1016805 "OpenShift stance on outgoing sockets"