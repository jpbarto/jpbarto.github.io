---
layout: single
status: publish
published: true
title: Using Nginx to build a site of sites
author:
  display_name: Jason Barto
  login: jpbarto
  email: jason.p.barto@gmail.com
  url: ''
author_login: jpbarto
author_email: jason.p.barto@gmail.com
excerpt: "<a title=\"Nginx.org\" href=\"http:&#47;&#47;nginx.org&#47;\" target=\"_blank\">Nginx<&#47;a>
  (pronounced engine-x) is a high performance asynchronous swiss-army knife of a TCP
  server. &nbsp;It is considered to be a web server, load balancer, caching proxy,
  and reverse&nbsp;proxy for HTTP&#47;S, IMAP, POP3 and SMTP. &nbsp;Nginx is the 2nd
  most used production server hosting internet sites today and has been in active
  development since 2002. &nbsp;Here at R9labs Nginx is used to pull together under
  a single domain a number of individual HTTP servers, not to mention securing them
  using <a title=\"OAuth Community Site\" href=\"http:&#47;&#47;oauth.net&#47;\">OAuth<&#47;a>
  authentication (but that's another post).\r\n\r\nIn this post we'll be detailing
  how we have used Nginx to bind together, using sub-domains, the many web consoles
  that we use day to day.\r\n\r\n"
wordpress_id: 4
wordpress_url: https://www.r9labs.org/?p=4
date: '2015-03-30 21:29:04 -0400'
date_gmt: '2015-03-30 21:29:04 -0400'
categories:
- Software
- Technical
tags:
- nginx
comments: []
---
<p><a title="Nginx.org" href="http:&#47;&#47;nginx.org&#47;" target="_blank">Nginx<&#47;a> (pronounced engine-x) is a high performance asynchronous swiss-army knife of a TCP server. &nbsp;It is considered to be a web server, load balancer, caching proxy, and reverse&nbsp;proxy for HTTP&#47;S, IMAP, POP3 and SMTP. &nbsp;Nginx is the 2nd most used production server hosting internet sites today and has been in active development since 2002. &nbsp;Here at R9labs Nginx is used to pull together under a single domain a number of individual HTTP servers, not to mention securing them using <a title="OAuth Community Site" href="http:&#47;&#47;oauth.net&#47;">OAuth<&#47;a> authentication (but that's another post).</p>
<p>In this post we'll be detailing how we have used Nginx to bind together, using sub-domains, the many web consoles that we use day to day.</p>
<p><a id="more"></a><a id="more-4"></a></p>
<p>More and more servers and software today seem to come with a web front end for administration and configuration either baked in or as an optional plugin. &nbsp;Generally this makes sense, a graphical UI allows users to more quickly begin using a database or application server without having to learn a complex command language or sifting through stacks of documentation about configuration files and parameters. &nbsp;Being able to quickly configure a software package, interact with it and get it behaving to a level where the user can begin using it, rather than spending days on installation and configuration, means that a user can stay focused on their ultimate goal which is why they are using a software package in the first place - to accomplish something (other than install a database).</p>
<p>Some examples of such software packages include <a title="RabbitMQ" href="http:&#47;&#47;www.rabbitmq.com&#47;" target="_blank">RabbitMQ<&#47;a>, <a title="Couchbase" href="http:&#47;&#47;www.couchbase.com&#47;" target="_blank">Couchbase<&#47;a>, <a title="Apache Storm" href="https:&#47;&#47;storm.apache.org&#47;" target="_blank">Storm<&#47;a> and <a title="Glassfish Application Server" href="https:&#47;&#47;glassfish.java.net&#47;" target="_blank">Glassfish<&#47;a>. &nbsp;All of these servers, when deployed, start an embedded web server which hosts web pages and allows a user to quickly <em>see<&#47;em> the system functioning, perhaps tweak a few parameters, and move on, occasionally glancing back to see the server is still performing as expected. &nbsp;Additionally once in operation these same web UIs can be used to manage, monitor and maintain the server systems.</p>
<p><strong>The Downside<&#47;strong></p>
<p>It bears mentioning that in a production environment these same conveniences (web UIs) can be considered a security risk as hosting a secure web server is not generally the primary focus of a complex event processor or database. &nbsp;In some instances, for this reason, the ability to disable such a web UI is a boon; for example the RabbitMQ web UI is a plugin which can be disabled when considered a risk or otherwise deemed unnecessary. &nbsp;Aside from the security risk posed, users of large complex systems may consider it more convenient to pull together onto a single dashboard information from multiple source systems and servers, leaving each server's individual web console behind for a more consolidated approach. &nbsp;In this instance the web UI again becomes, generally, unnecessary however a <a title="RESTful APIs" href="http:&#47;&#47;www.restapitutorial.com&#47;" target="_blank">REST<&#47;a> interface for information retrieval and interaction is a huge plus.</p>
<p><strong>A Sum of Parts<&#47;strong></p>
<p>Getting back to R9labs we use many servers and software systems day to day as part of our projects and Nginx makes it possible to host these under a single domain or set of sub-domains rather than a random collection of IP addresses or, worse, port numbers; ever tried to develop a web app running multiple web and application servers all on a single system? &nbsp;Thankfully gone are the days of localhost:80, localhost:8080, localhost:8088, localhost:15672 and so on.</p>
<p>For the following examples we will describe a system which uses <a title="Supervisord" href="http:&#47;&#47;supervisord.org&#47;" target="_blank">Supervisord<&#47;a> to monitor system processes, Couchbase for a key-value store, RabbitMQ for a messaging broker and Nginx itself for serving <a title="PHP Hypertext Preprocessor" href="http:&#47;&#47;php.net&#47;" target="_blank">PHP<&#47;a> pages.</p>
<p>These 4 systems create web consoles that listen (by default) for browser connections on the following ports:</p>
<ul>
<li>Supervisord listens on port 9001<&#47;li>
<li>RabbitMQ listens on port 15672<&#47;li>
<li>Couchbase listens on port 8091<&#47;li>
<li>Nginx listens on port 80<&#47;li><br />
<&#47;ul><br />
During the systems development we will want to be able to access the main site at r9labs.net (Nginx), Supervisord at ps.r9labs.net, RabbitMQ at mq.r9labs.net and Couchbase at db.r9labs.net. &nbsp;First lets configure Nginx to host a collection of PHP pages at r9labs.net.</p>
<p><strong>Nginx and R9labs.net<&#47;strong></p>
<p>Nginx, under <a title="Ubuntu Linux" href="http:&#47;&#47;www.ubuntu.com&#47;" target="_blank">Ubuntu linux,<&#47;a> has its configuration files under <span class="lang:sh decode:true  crayon-inline ">&#47;etc&#47;nginx<&#47;span>&nbsp;. &nbsp;For our purposes we will be focusing on <span class="lang:sh decode:true  crayon-inline">&#47;etc&#47;nginx&#47;nginx.conf,<&#47;span>&nbsp;&nbsp;the files in <span class="lang:sh decode:true  crayon-inline ">&#47;etc&#47;nginx&#47;conf.d<&#47;span>&nbsp;&nbsp;and <span class="lang:sh decode:true  crayon-inline ">&#47;etc&#47;nginx&#47;sites-enabled<&#47;span>&nbsp;. <span style="color: #993366">A quick disclaimer - this post is not meant to be an exhaustive tutorial on configuring Nginx, there are a great many <a href="http:&#47;&#47;nginx.org&#47;en&#47;books.html" target="_blank">books<&#47;a>, <a href="http:&#47;&#47;nginx.org&#47;en&#47;docs&#47;" target="_blank">websites<&#47;a> and <a href="https:&#47;&#47;www.digitalocean.com&#47;community&#47;tags&#47;nginx?primary_filter=tutorials" target="_blank">tutorials<&#47;a> dedicated to such goals; this post will only show those changes within an Nginx configuration that need to be in place to enable it to proxy other web servers.<&#47;span></p>
<p><span style="color: #993366">Further disclaimer: this post was authored using Ubuntu Server v14.04. &nbsp;Please bear this in mind as file locations and even files themselves may change or move depending upon your host operating system.<&#47;span></p>
<p>For anyone accustomed to configuring <a title="Apache httpd" href="http:&#47;&#47;httpd.apache.org&#47;" target="_blank">Apache web server<&#47;a> many of the configuration directives fed to Nginx will look very familiar. &nbsp;The first website of our&nbsp;system will be accessible from http:&#47;&#47;www.r9labs.net and so to configure it we will create a site configuration file at <span class="lang:sh decode:true  crayon-inline ">&#47;etc&#47;nginx&#47;sites-enabled&#47;www<&#47;span>&nbsp;. &nbsp;In this file will be all the parameters needed to configure Nginx to host a PHP website at http:&#47;&#47;www.r9labs.net.</p>
<p>To start we create a <span class="lang:c decode:true  crayon-inline">server {}<&#47;span> section to contain and represent the initial site server. &nbsp;Within this section&nbsp;we will begin with some basic directives such as which IP address and port to listen on, the root document directory, the web server name and what constitutes an '<em>index<&#47;em>' page:</p>
<pre class="lang:c decode:true">server {<br />
    listen 192.168.10.102:80 default_server;<br />
    root &#47;var&#47;www&#47;html;<br />
    index index.php index.html index.htm;<br />
    server_name www.r9labs.net;<br />
}<&#47;pre><br />
To host PHP scripts we will use <a title="FastCGI" href="http:&#47;&#47;www.fastcgi.com&#47;" target="_blank">FastCGI<&#47;a> to execute the PHP script and return the output (a rendered page) to Nginx for delivery to the user.</p>
<pre class="lang:c decode:true">location ~ \.php$ {<br />
    fastcgi_split_path_info ^(.+\.php)(&#47;.+)$;<br />
    try_files $uri $uri&#47; =404;<br />
    fastcgi_pass unix:&#47;var&#47;run&#47;php5-fpm.sock;<br />
    fastcgi_index index.php;<br />
    fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;<br />
    include fastcgi_params;<br />
}<&#47;pre><br />
This location section matches any page ending in '<em>.php<&#47;em>' and hands off execution of the requested PHP resource to FastCGI. &nbsp;These directives instruct the Nginx server to communicate with FastCGI over a file-based socket on the local server, specifies the PHP index filename and the requested resource. &nbsp;The <span class="lang:c decode:true  crayon-inline ">try_files<&#47;span>&nbsp;&nbsp;directive helps to provide minimal protection against a <a href="https:&#47;&#47;nealpoole.com&#47;blog&#47;2011&#47;04&#47;setting-up-php-fastcgi-and-nginx-dont-trust-the-tutorials-check-your-configuration&#47;" target="_blank">well known vulnerability<&#47;a> where Nginx could be tricked into executing arbitrary PHP code.</p>
<p>In addition to the PHP location section we will also include 2 additional location sections, the first is a basic root location and the second is to prevent Nginx from serving up any '.ht' files such as .htaccess:</p>
<pre class="lang:c decode:true">location &#47; {<br />
    try_files $uri $uri&#47; &#47;index.html &#47;index.php?$args;<br />
}</p>
<p>location ~ &#47;\.ht {<br />
    deny all;<br />
}<&#47;pre><br />
The final www site configuration is:</p>
<pre class="lang:c decode:true" title="&#47;etc&#47;nginx&#47;sites-enabled&#47;www">server {<br />
    listen 192.168.10.102:80 default_server;</p>
<p>    root &#47;var&#47;www&#47;html;<br />
    index index.php index.html index.htm;</p>
<p>    server_name www.r9labs.net;</p>
<p>    location ~ \.php$ {<br />
        fastcgi_split_path_info ^(.+\.php)(&#47;.+)$;<br />
        try_files $uri $uri&#47; =404;<br />
        fastcgi_pass unix:&#47;var&#47;run&#47;php5-fpm.sock;<br />
        fastcgi_index index.php;<br />
        fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;<br />
        include fastcgi_params;<br />
    } </p>
<p>    location &#47; {<br />
        try_files $uri $uri&#47; &#47;index.html &#47;index.php?$args;<br />
    } </p>
<p>    location ~ &#47;\.ht {<br />
        deny all;<br />
    }<br />
}<&#47;pre><br />
<strong>RabbitMQ and Nginx<&#47;strong></p>
<p>As we know Nginx is prepared to behave as a web server itself, responding to HTTP requests as they roll in. &nbsp;But with other web servers running behind Nginx we now want it to forward some requests backward to the other servers hosting varying portions of the R9labs.net intranetwork site. &nbsp;To do this we need to first create what Nginx calls an 'upstream' server. &nbsp;RabbitMQ hosts a web management console on an in-built web server accessible on port 15672. &nbsp;To inform Nginx of the RabbitMQ server we create <span class="lang:sh decode:true  crayon-inline ">&#47;etc&#47;nginx&#47;conf.d&#47;upstream.conf<&#47;span>&nbsp;&nbsp;and enter into this file an upstream server entry for RabbitMQ:</p>
<pre class="lang:c decode:true" title="&#47;etc&#47;nginx&#47;conf.d&#47;upstream.conf">upstream rabbitmq-mgmt-console {<br />
&amp;nbsp; &amp;nbsp; server 127.0.0.1:15672;<br />
}<&#47;pre><br />
The format of this configuration directive is</p>
<pre class="lang:c decode:true">upstream <upstream server name> {<br />
&nbsp; &nbsp; server s1.example.com:port;<br />
&nbsp; &nbsp; server s2.example.com:port;<br />
&nbsp; &nbsp; server s3.example.com:port;<br />
}<&#47;pre><br />
The '<upstream server name>' will be used in later configuration files to reference back to this upstream definition and every 'server' entry will become a candidate for servicing requests directed to this upstream service. &nbsp;For more information on defining upstream servers see <a href="http:&#47;&#47;nginx.org&#47;en&#47;docs&#47;http&#47;ngx_http_upstream_module.html" target="_blank">Nginx's Upstream documentation<&#47;a>.</p>
<p>One thing to note is that as a small security measure we have all backend servers listening only to the localhost address of 127.0.0.1. &nbsp;This will force all external clients to address their requests to Nginx rather than speaking directly with the upstream servers.</p>
<p>With the upstream server defined the next step is to instruct Nginx to forward traffic to the upstream RabbitMQ instance. &nbsp;We create another 'sites' configuration file to do this at '&#47;etc&#47;nginx&#47;sites-enabled&#47;rabbitmq':</p>
<pre class="lang:c decode:true " title="&#47;etc&#47;nginx&#47;sites-enabled&#47;rabbitmq">server {<br />
    listen 192.168.10.102:80;</p>
<p>    server_name mq.r9labs.net;</p>
<p>    location &#47; {<br />
        proxy_pass http:&#47;&#47;rabbitmq-mgmt-console;<br />
        proxy_set_header Host $host;<br />
        proxy_set_header X-Real-IP $remote_addr;<br />
        proxy_set_header X-Scheme $scheme;<br />
        proxy_connect_timeout 1;<br />
    }<br />
}<&#47;pre><br />
The key directives in this site file are the proxy directives which instruct Nginx to forward HTTP requests onto the upstream server 'rabbitmq-mgmt-console' using the familiar URI scheme of 'http:&#47;&#47;'. &nbsp;By saving this configruation file and restarting Nginx the RabbitMQ management console should now be available at http:&#47;&#47;mq.rabbitmq.net with the main PHP website available on http:&#47;&#47;www.r9labs.net.</p>
<p><strong>Additional Servers<&#47;strong></p>
<p>This process of creating an 'upstream' server and then defining a site file for the upstream server can be followed almost indefinitely to pull together under a single domain a collection of servers and services all being brokered by Nginx. &nbsp;As was metnioned earlier this development application needs to have its management consoles available through a couple of different URIs. &nbsp;To do this the final <span class="lang:sh decode:true  crayon-inline ">upstream.conf<&#47;span>&nbsp;&nbsp;file looks like:</p>
<pre class="lang:c decode:true" title="&#47;etc&#47;nginx&#47;conf.d&#47;upstream.conf">upstream supervisord {<br />
    server 127.0.0.1:9001;<br />
}</p>
<p>upstream rabbitmq-mgmt-console {<br />
    server 127.0.0.1:15672;<br />
}</p>
<p>upstream couchbase-console {<br />
    server 127.0.0.1:8091;<br />
}<&#47;pre><br />
Each of the 2 remaining sites also get a configuration file similar to that of the RabbitMQ management console in order to map the sub-domain URI to the upstream server. &nbsp;For completeness sake:</p>
<pre class="lang:c decode:true " title="&#47;etc&#47;nginx&#47;sites-enabled&#47;couchbase">server {<br />
    listen 192.168.10.102:80;</p>
<p>    server_name db.r9labs.net;</p>
<p>    location &#47; {<br />
        proxy_pass http:&#47;&#47;couchbase-console;<br />
        proxy_set_header Host $host;<br />
        proxy_set_header X-Real-IP $remote_addr;<br />
        proxy_set_header X-Scheme $scheme;<br />
        proxy_connect_timeout 1;<br />
    }<br />
}<&#47;pre><br />
And...</p>
<pre class="lang:default decode:true " title="&#47;etc&#47;nginx&#47;sites-enabled&#47;supervisord">server {<br />
    listen 192.168.10.102:80;</p>
<p>    server_name su.r9labs.net;</p>
<p>    location &#47; {<br />
        proxy_pass http:&#47;&#47;supervisord;<br />
        proxy_set_header Host $host;<br />
        proxy_set_header X-Real-IP $remote_addr;<br />
        proxy_set_header X-Scheme $scheme;<br />
        proxy_connect_timeout 1;<br />
    }<br />
}<&#47;pre><br />
With the aforementioend configuration files in place and Nginx restarted developers are now free to communicate with various system components by using relatively static human-readable hostnames such as db.r9labs.net and mq.r9labs.net and removes the need for the developers to memorize a long list of standard port numbers for each service.</p>
<p>Thank you and I hope you've found this article useful.</p>
<p>Sincerely,</p>
<p>Jason</p>
<p>&nbsp;</p>
