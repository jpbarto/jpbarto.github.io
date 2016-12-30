---
title: Authenticate your services with Google, NGINX, and OAuth2
layout: single
published: true
categories:
  - Technical
  - Technology
tags:
  - google
  - nginx
  - oauth
  - oauth2
  - proxy
  - supervisor
---
<img class="aligncenter" src="https://developers.google.com/accounts/images/sign-in-with-google.png" alt="Sign in with Google" width="492" height="108" />

Many web UIs come with only basic multi-tenancy.  Some web applications come with no security at all.  A very quick, simple example is the IPython Notebook, a web-based python environment, often used as part of the data science process, which has no requirement for authentication of users as it is intended as a single-user web application and was never meant to be hosted publicly. (I recognize that there are multi-tenant Notebook systems out there, but include IPython Notebook as an example)

Even web applications that do allow for authentication, authorization, and accounting &#8211; the three As of multi-tenancy, will often only support HTTP basic authentication, private (ie application-only / application-specific) user databases, or LDAP authentication.   But the web has moved on.  The need to secure web applications, RESTful HTTP endpoints, and web services, not to mention the desire for true single sign-on, has led to the development of technology and standards such as Security Assertion Markup Language (SAML) and OAuth (and ultimately OAuth2).

This article will focus on OAuth2 and how, along with an open source proxy project, it can be used to secure applications which are unable to natively authenticate users using OAuth2.

<!--more-->

## OAuth2 in the Nutshell

OAuth2 is the successor to OAuth, originally created in 2006.  It allows for the authentication of users via a trusted 3rd party.  Today it is not uncommon to log into a website (https://bbc.co.uk for instance) using a Facebook or Google account, or some other 3rd party prominent website with a large number of users. When a user chooses to log into a website using their Google account, the website will begin a backoffice authentication process with Google&#8217;s servers, requesting the user&#8217;s information.  In this instance the user will authenticate themselves with Google&#8217;s servers and Google&#8217;s servers will issue a token to the target website, vouching that the user is who they claim to be.  This scenario provides added security in that the user no longer has to remember a multitude of usernames and passwords, and can instead authenticate him or herself securely using a single set of credentials.

For a more detailed discussion around the inner workings of OAuth2 I recommend doing a quick [Web search](https://www.google.co.uk/search?q=oauth2+explained) or starting with the following link to learn more: [OAuth2 Simplified](https://aaronparecki.com/2012/07/29/2/oauth2-simplified).

## Limited OAuth2 Support

As has been stated the number of webapps which are OAuth aware is still very low. As a result it may be useful to leverage a proxy to act as the login manager for your application. For the remainder, this article will describe a scenario that places an instance of [Supervisor](http://supervisord.org/) behind a proxy which is OAuth aware. When a user attempts to access the Supervisor Web interface the user will first be authenticated via the OAuth proxy before being forwarded to the Supervisor interface. Even if Supervisor does nothing with the authenticated users information a greater level of protection of the Supervisor service is being afforded above a more traditional method such as HTTP Basic authentication.

## Article Focus

The proxy of choice in this article is the [OAuth2 Proxy](https://github.com/bitly/oauth2_proxy) written in [Go](https://golang.org/) by the [Bit.ly team](http://word.bitly.com/) and hosted on [GitHub](https://github.com/bitly/oauth2_proxy).  Along with the configuration of [NGINX](https://www.nginx.com/) and Supervisor, this article will focus on the installation and configuration of the OAuth2 Proxy to authenticate users against Google&#8217;s OAuth service.  When configured a user&#8217;s HTTP / HTTPS request will flow as follows:

User &#8211;> NGINX &#8211;> OAuth2 Proxy &#8211;> Supervisor Web UI

NGINX will listen on the external IP address of a web server and forward requests to an upstream OAuth2 Proxy listening on the loopback address.  Subsequently the proxy will forward authenticated requests upstream to the Supervisor which is also running on the loopback address.

## Assumptions

This article assumes that NGINX and Supervisor are installed as services on your server. It also assumes that Supervisor is running with its Web UI enabled.  For more information on how to configure Supervisor to enable its Web UI see this bit of the [project&#8217;s documentation](http://supervisord.org/configuration.html#inet-http-server-section-settings).

## OAuth2 Proxy Overview

OAuth2 Proxy is, from the project&#8217;s Readme,

> A reverse proxy and static file server that provides authentication using Providers (Google, Github, and others) to validate accounts by email, domain or group.

Bit.ly&#8217;s OAuth2 Proxy is capable of authenticating against the OAuth services of Google, Github, Azure, Gitlab, LinkedIn, and MyUSA.  It works by intercepting (read: proxying) an HTTP request intended for a guarded service, first authenticates the request against a specified OAuth2 identity provider, and then, presuming the authentication is successful, stores the OAuth2 cookie into a browser cookie.  The proxy server has matured drastically even in just the last year and has a multitude of features and options that should support most deployment scenarios.

## Preparation

This article will walk through the following process for deploying and configuring the components previously mentioned.  The process will be as follows:

  1. Prepare Google&#8217;s OAuth service for use by the proxy
  2. Install the OAuth2 Proxy
  3. Configure the OAuth2 Proxy to forward HTTP requests from an internal port to the desired service port, in this case the Supervisor HTTP server port
  4. Start the OAuth2 Proxy using Supervisor (this could just as easily be done by installing the proxy as an OS-level service)
  5. Finally configure NGINX to route messages to the OAuth2 Proxy

Note: NGINX is not strictly required in this case, however as this is being used as part of an Internet-facing server I consider NGINX to be far more battle-tested and hardened against malicious Internet traffic when compared to the OAuth2 Proxy or Supervisord.  As such NGINX will act as a first, and admittedly very thin, line of defense against malicious traffic.

## Step 1: Prepare Google&#8217;s OAuth service

In order for the OAuth proxy to communicate with Google&#8217;s OAuth service the proxy will require a client ID and client secret.  To create the ID and secret a project needs to first be created with Google using the [Google Developer&#8217;s console](https://console.developers.google.com/project).  From the developer&#8217;s console create a new project, giving it some meaningful name, and next choose the newly created project from the top right project dropdown. In the project&#8217;s Dashboard choose &#8216;Enable and manage APIs&#8217;.  Next click &#8216;Credentials&#8217; in the left navigation pane followed by clicking the &#8216;OAuth consent screen&#8217; tab.  Fill in &#8216;Product name show to users&#8217; with a meaningful product name, in this case &#8216;Supervisor&#8217; will suffice.  Click &#8216;Save&#8217;.

Now in the center pane select the &#8216;Credentials&#8217; tab and open the &#8216;New Credentials&#8217; drop down.  Choose OAuth client ID and choose &#8216;web application&#8217;.  Enter an appropriate application name, such as &#8216;supervisor\_oauth\_proxy&#8217; and enter your site&#8217;s domain name for &#8216;Authorized JavaScript origins&#8217;, for example https://supervisor.example.com.  Enter into &#8216;Authorized redirect URIs&#8217; the location of the OAuth2 callback, this is typically something like https://supervisor.example.com/oauth2/callback.  Finally click &#8216;Create&#8217; and record the Client ID and Client Secret generated by Google as these will need to be given to the OAuth proxy.

## Step 2: OAuth2 Proxy Installation

These days the project has pre-compiled binaries available for distribution meaning that it is no longer necessary to learn Go build tools or configure a Go build environment in order to use the Proxy.  If you have intentions of contributing to the project or just want to see how the project operates under the hood you&#8217;re invited to fork or clone the repository as per the typical Github process.  However if you just want to be a user of the proxy [obtaining a binary distribution](https://github.com/bitly/oauth2_proxy/releases) will do just fine.  As such download and extract the binary into an appropriate directory such as /opt/oauth2\_proxy.  You should now have a binary named &#8216;oauth2\_proxy&#8217; located at /opt/oauth2\_proxy/oauth2\_proxy&#8217;.

## Step 3: OAuth2 Proxy Configuration

To hold the configuration of the OAuth2 Proxy and to provide it with a database of approved user email addresses create the directory /opt/oauth2_proxy/etc.  This directory will hold the configuration file for the proxy server along with the database of approved email addresses that can log into the Supervisor web UI.

First create the email database, it is a plain text file with an email address per line:

<pre class="lang:default decode:true " title="/opt/oauth2_proxy/etc/authorized_emails">john.doe@example.com
sally.jessie@gmail.com
roger.moore@gmail.com</pre>

Next create a configuration file for the proxy server:

<pre class="lang:default decode:true" title="/opt/oauth2_proxy/etc/supervisor_oauth_proxy.conf"># IP address and port on which the proxy should listen for requests
http_address = "127.0.0.1:4181"
# the IP address, port, and protocol of the Supervisor server
upstreams = [ "http://127.0.0.1:9001" ]
request_logging = true
client_id = "insert_google_client_id_here"
client_secret = "insert_google_client_secret_here"
authenticated_emails_file = "/opt/oauth2_proxy/etc/authenticated_emails"
cookie_secret = "secret_passphrase"</pre>

For a complete list of the configuration directives please see the project page for OAuth2 Proxy.

The directives included in the configuration file above are the key parameters to support this particular use case, however you are encouraged to visit the project home page to find out more about the many configurable behaviors of the proxy server.

After authenticating a user via the Google service the OAuth proxy will check whether the authenticated user is authorized to access the service by looking for the user&#8217;s email address in the email address database identified by the &#8211;authenticated-emails-file directive.

To confirm the proxy configuration is valid execute the following command to start the proxy server.  Presuming it logs no errors or warnings you can issue a &#8216;Ctrl-C&#8217; to stop the proxy server and move on to launching the proxy server from Supervisor.

<pre class="lang:sh decode:true" title="Configuration Validation Command">$ /opt/oauth2_proxy/oauth2_proxy -config /opt/oauth2_proxy/etc/supervisor_oauth2_proxy.conf</pre>

## Step 4: Configure Supervisor

To have Supervisor launch and manage the OAuth Proxy on startup a program will need to be added to the Supervisor configuration.  In order to do this create a configuration file for the OAuth2 Proxy in Supervisor&#8217;s conf.d directory:

<pre class="lang:default decode:true " title="/etc/supervisor/conf.d/supervisor-oauth.conf">[program:supervisor_oauth]
command=/opt/oauth2_proxy/oauth2_proxy -config=/opt/oauth2_proxy/etc/supervisor_oauth_proxy.conf
numprocs=1
user=nobody
stopsignal=TERM
autostart=true
autorestart=true
redirect_stderr=true
stdout_logfile=auto</pre>

Now reload the Supervisor service and the OAuth2 proxy should be up and listening for requests.  The next step is to configure NGINX to forward requests to the proxy server as an upstream destination.

## Step 5: Configure NGINX

In order to configure NGINX add an upstream server to the NGINX configuration:

<pre class="lang:default decode:true" title="/etc/nginx/conf.d/upstream.conf">...
upstream supervisor-oauth-proxy {
    server 127.0.0.1:4181;
}</pre>

Next add a Supervisor configuration file to NGINX&#8217;s sites-available directory:

<pre class="lang:default decode:true" title="SupervisorD site configuration for NGINX">server {
server_name supervisor.example.com;
listen 192.168.103.104:80;
return 301 https://$host$request_uri;
}

server {
    listen 192.168.103.104:443 ssl;

    ssl_certificate     /etc/ssl/certs/example.com.pub.pem;
    ssl_certificate_key /etc/ssl/private/example.com.priv.pem;
    ssl_protocols       SSLv3 TLSv1 TLSv1.1 TLSv1.2;
    ssl_ciphers         HIGH:!aNULL:!MD5;
    add_header Strict-Transport-Security max-age=1209600;

    root /var/www/html;
    index index.html index.htm;

    server_name super.example.com;

    location / {
        proxy_pass http://supervisor-oauth-proxy;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Scheme $scheme;
        proxy_connect_timeout 1;
    }
}</pre>

After reloading the NGINX service you should now be able to access http://supervisor.example.com and be presented with an option to log in via Google thanks to the OAuth2 proxy.  After successfully logging in, presuming your email address is in the approved emails database, you should be forwarded to the Supervisor web interface.

&nbsp;

Enjoy
