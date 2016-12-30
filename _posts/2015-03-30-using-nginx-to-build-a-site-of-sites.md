---
id: 4
title: Using Nginx to build a site of sites
date: 2015-03-30T21:29:04+00:00
author: Jason Barto
layout: single
guid: https://www.r9labs.org/?p=4
permalink: /using-nginx-to-build-a-site-of-sites/
categories:
  - Software
  - Technical
tags:
  - nginx
---
<a title="Nginx.org" href="http://nginx.org/" target="_blank">Nginx</a> (pronounced engine-x) is a high performance asynchronous swiss-army knife of a TCP server.  It is considered to be a web server, load balancer, caching proxy, and reverse proxy for HTTP/S, IMAP, POP3 and SMTP.  Nginx is the 2nd most used production server hosting internet sites today and has been in active development since 2002.  Here at R9labs Nginx is used to pull together under a single domain a number of individual HTTP servers, not to mention securing them using [OAuth](http://oauth.net/ "OAuth Community Site") authentication (but that&#8217;s another post).

In this post we&#8217;ll be detailing how we have used Nginx to bind together, using sub-domains, the many web consoles that we use day to day.

<!--more-->

More and more servers and software today seem to come with a web front end for administration and configuration either baked in or as an optional plugin.  Generally this makes sense, a graphical UI allows users to more quickly begin using a database or application server without having to learn a complex command language or sifting through stacks of documentation about configuration files and parameters.  Being able to quickly configure a software package, interact with it and get it behaving to a level where the user can begin using it, rather than spending days on installation and configuration, means that a user can stay focused on their ultimate goal which is why they are using a software package in the first place &#8211; to accomplish something (other than install a database).

Some examples of such software packages include <a title="RabbitMQ" href="http://www.rabbitmq.com/" target="_blank">RabbitMQ</a>, <a title="Couchbase" href="http://www.couchbase.com/" target="_blank">Couchbase</a>, <a title="Apache Storm" href="https://storm.apache.org/" target="_blank">Storm</a> and <a title="Glassfish Application Server" href="https://glassfish.java.net/" target="_blank">Glassfish</a>.  All of these servers, when deployed, start an embedded web server which hosts web pages and allows a user to quickly _see_ the system functioning, perhaps tweak a few parameters, and move on, occasionally glancing back to see the server is still performing as expected.  Additionally once in operation these same web UIs can be used to manage, monitor and maintain the server systems.

**The Downside**

It bears mentioning that in a production environment these same conveniences (web UIs) can be considered a security risk as hosting a secure web server is not generally the primary focus of a complex event processor or database.  In some instances, for this reason, the ability to disable such a web UI is a boon; for example the RabbitMQ web UI is a plugin which can be disabled when considered a risk or otherwise deemed unnecessary.  Aside from the security risk posed, users of large complex systems may consider it more convenient to pull together onto a single dashboard information from multiple source systems and servers, leaving each server&#8217;s individual web console behind for a more consolidated approach.  In this instance the web UI again becomes, generally, unnecessary however a <a title="RESTful APIs" href="http://www.restapitutorial.com/" target="_blank">REST</a> interface for information retrieval and interaction is a huge plus.

**A Sum of Parts**

Getting back to R9labs we use many servers and software systems day to day as part of our projects and Nginx makes it possible to host these under a single domain or set of sub-domains rather than a random collection of IP addresses or, worse, port numbers; ever tried to develop a web app running multiple web and application servers all on a single system?  Thankfully gone are the days of localhost:80, localhost:8080, localhost:8088, localhost:15672 and so on.

For the following examples we will describe a system which uses <a title="Supervisord" href="http://supervisord.org/" target="_blank">Supervisord</a> to monitor system processes, Couchbase for a key-value store, RabbitMQ for a messaging broker and Nginx itself for serving <a title="PHP Hypertext Preprocessor" href="http://php.net/" target="_blank">PHP</a> pages.

These 4 systems create web consoles that listen (by default) for browser connections on the following ports:

  * Supervisord listens on port 9001
  * RabbitMQ listens on port 15672
  * Couchbase listens on port 8091
  * Nginx listens on port 80

During the systems development we will want to be able to access the main site at r9labs.net (Nginx), Supervisord at ps.r9labs.net, RabbitMQ at mq.r9labs.net and Couchbase at db.r9labs.net.  First lets configure Nginx to host a collection of PHP pages at r9labs.net.

**Nginx and R9labs.net**

Nginx, under <a title="Ubuntu Linux" href="http://www.ubuntu.com/" target="_blank">Ubuntu linux,</a> has its configuration files under <span class="lang:sh decode:true  crayon-inline ">/etc/nginx</span> .  For our purposes we will be focusing on <span class="lang:sh decode:true  crayon-inline">/etc/nginx/nginx.conf,</span>  the files in <span class="lang:sh decode:true  crayon-inline ">/etc/nginx/conf.d</span>  and <span class="lang:sh decode:true  crayon-inline ">/etc/nginx/sites-enabled</span> . <span style="color: #993366">A quick disclaimer &#8211; this post is not meant to be an exhaustive tutorial on configuring Nginx, there are a great many <a href="http://nginx.org/en/books.html" target="_blank">books</a>, <a href="http://nginx.org/en/docs/" target="_blank">websites</a> and <a href="https://www.digitalocean.com/community/tags/nginx?primary_filter=tutorials" target="_blank">tutorials</a> dedicated to such goals; this post will only show those changes within an Nginx configuration that need to be in place to enable it to proxy other web servers.</span>

<span style="color: #993366">Further disclaimer: this post was authored using Ubuntu Server v14.04.  Please bear this in mind as file locations and even files themselves may change or move depending upon your host operating system.</span>

For anyone accustomed to configuring <a title="Apache httpd" href="http://httpd.apache.org/" target="_blank">Apache web server</a> many of the configuration directives fed to Nginx will look very familiar.  The first website of our system will be accessible from http://www.r9labs.net and so to configure it we will create a site configuration file at <span class="lang:sh decode:true  crayon-inline ">/etc/nginx/sites-enabled/www</span> .  In this file will be all the parameters needed to configure Nginx to host a PHP website at http://www.r9labs.net.

To start we create a <span class="lang:c decode:true  crayon-inline">server {}</span> section to contain and represent the initial site server.  Within this section we will begin with some basic directives such as which IP address and port to listen on, the root document directory, the web server name and what constitutes an &#8216;_index_&#8216; page:

<pre class="lang:c decode:true">server {
    listen 192.168.10.102:80 default_server;
    root /var/www/html;
    index index.php index.html index.htm;
    server_name www.r9labs.net;
}</pre>

To host PHP scripts we will use <a title="FastCGI" href="http://www.fastcgi.com/" target="_blank">FastCGI</a> to execute the PHP script and return the output (a rendered page) to Nginx for delivery to the user.

<pre class="lang:c decode:true">location ~ \.php$ {
    fastcgi_split_path_info ^(.+\.php)(/.+)$;
    try_files $uri $uri/ =404;
    fastcgi_pass unix:/var/run/php5-fpm.sock;
    fastcgi_index index.php;
    fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
    include fastcgi_params;
}</pre>

This location section matches any page ending in &#8216;_.php_&#8216; and hands off execution of the requested PHP resource to FastCGI.  These directives instruct the Nginx server to communicate with FastCGI over a file-based socket on the local server, specifies the PHP index filename and the requested resource.  The <span class="lang:c decode:true  crayon-inline ">try_files</span>  directive helps to provide minimal protection against a <a href="https://nealpoole.com/blog/2011/04/setting-up-php-fastcgi-and-nginx-dont-trust-the-tutorials-check-your-configuration/" target="_blank">well known vulnerability</a> where Nginx could be tricked into executing arbitrary PHP code.

In addition to the PHP location section we will also include 2 additional location sections, the first is a basic root location and the second is to prevent Nginx from serving up any &#8216;.ht&#8217; files such as .htaccess:

<pre class="lang:c decode:true">location / {
    try_files $uri $uri/ /index.html /index.php?$args;
}

location ~ /\.ht {
    deny all;
}</pre>

The final www site configuration is:

<pre class="lang:c decode:true" title="/etc/nginx/sites-enabled/www">server {
    listen 192.168.10.102:80 default_server;

    root /var/www/html;
    index index.php index.html index.htm;

    server_name www.r9labs.net;

    location ~ \.php$ {
        fastcgi_split_path_info ^(.+\.php)(/.+)$;
        try_files $uri $uri/ =404;
        fastcgi_pass unix:/var/run/php5-fpm.sock; 
        fastcgi_index index.php; 
        fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name; 
        include fastcgi_params; 
    } 

    location / { 
        try_files $uri $uri/ /index.html /index.php?$args; 
    } 

    location ~ /\.ht { 
        deny all; 
    } 
}</pre>

**RabbitMQ and Nginx**

As we know Nginx is prepared to behave as a web server itself, responding to HTTP requests as they roll in.  But with other web servers running behind Nginx we now want it to forward some requests backward to the other servers hosting varying portions of the R9labs.net intranetwork site.  To do this we need to first create what Nginx calls an &#8216;upstream&#8217; server.  RabbitMQ hosts a web management console on an in-built web server accessible on port 15672.  To inform Nginx of the RabbitMQ server we create <span class="lang:sh decode:true  crayon-inline ">/etc/nginx/conf.d/upstream.conf</span>  and enter into this file an upstream server entry for RabbitMQ:

<pre class="lang:c decode:true" title="/etc/nginx/conf.d/upstream.conf">upstream rabbitmq-mgmt-console {
&nbsp; &nbsp; server 127.0.0.1:15672;
}</pre>

The format of this configuration directive is

<pre class="lang:c decode:true">upstream &lt;upstream server name&gt; {
    server s1.example.com:port;
    server s2.example.com:port;
    server s3.example.com:port;
}</pre>

The &#8216;<upstream server name>&#8217; will be used in later configuration files to reference back to this upstream definition and every &#8216;server&#8217; entry will become a candidate for servicing requests directed to this upstream service.  For more information on defining upstream servers see <a href="http://nginx.org/en/docs/http/ngx_http_upstream_module.html" target="_blank">Nginx&#8217;s Upstream documentation</a>.

One thing to note is that as a small security measure we have all backend servers listening only to the localhost address of 127.0.0.1.  This will force all external clients to address their requests to Nginx rather than speaking directly with the upstream servers.

With the upstream server defined the next step is to instruct Nginx to forward traffic to the upstream RabbitMQ instance.  We create another &#8216;sites&#8217; configuration file to do this at &#8216;/etc/nginx/sites-enabled/rabbitmq&#8217;:

<pre class="lang:c decode:true " title="/etc/nginx/sites-enabled/rabbitmq">server {
    listen 192.168.10.102:80;

    server_name mq.r9labs.net;

    location / {
        proxy_pass http://rabbitmq-mgmt-console;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Scheme $scheme;
        proxy_connect_timeout 1;
    }
}</pre>

The key directives in this site file are the proxy directives which instruct Nginx to forward HTTP requests onto the upstream server &#8216;rabbitmq-mgmt-console&#8217; using the familiar URI scheme of &#8216;http://&#8217;.  By saving this configruation file and restarting Nginx the RabbitMQ management console should now be available at http://mq.rabbitmq.net with the main PHP website available on http://www.r9labs.net.

**Additional Servers**

This process of creating an &#8216;upstream&#8217; server and then defining a site file for the upstream server can be followed almost indefinitely to pull together under a single domain a collection of servers and services all being brokered by Nginx.  As was metnioned earlier this development application needs to have its management consoles available through a couple of different URIs.  To do this the final <span class="lang:sh decode:true  crayon-inline ">upstream.conf</span>  file looks like:

<pre class="lang:c decode:true" title="/etc/nginx/conf.d/upstream.conf">upstream supervisord {
    server 127.0.0.1:9001;
}

upstream rabbitmq-mgmt-console {
    server 127.0.0.1:15672;
}

upstream couchbase-console {
    server 127.0.0.1:8091;
}</pre>

Each of the 2 remaining sites also get a configuration file similar to that of the RabbitMQ management console in order to map the sub-domain URI to the upstream server.  For completeness sake:

<pre class="lang:c decode:true " title="/etc/nginx/sites-enabled/couchbase">server {
    listen 192.168.10.102:80;

    server_name db.r9labs.net;

    location / {
        proxy_pass http://couchbase-console;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Scheme $scheme;
        proxy_connect_timeout 1;
    }
}</pre>

And&#8230;

<pre class="lang:default decode:true " title="/etc/nginx/sites-enabled/supervisord">server {
    listen 192.168.10.102:80;

    server_name su.r9labs.net;

    location / {
        proxy_pass http://supervisord;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Scheme $scheme;
        proxy_connect_timeout 1;
    }
}</pre>

With the aforementioend configuration files in place and Nginx restarted developers are now free to communicate with various system components by using relatively static human-readable hostnames such as db.r9labs.net and mq.r9labs.net and removes the need for the developers to memorize a long list of standard port numbers for each service.

Thank you and I hope you&#8217;ve found this article useful.

Sincerely,

Jason

&nbsp;
