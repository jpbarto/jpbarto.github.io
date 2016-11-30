---
layout: single
status: publish
published: true
title: Authenticate your services with Google, NGINX, and OAuth2
author:
  display_name: Jason Barto
  login: jpbarto
  email: jason.p.barto@gmail.com
  url: ''
author_login: jpbarto
author_email: jason.p.barto@gmail.com
excerpt: "<img class=\"aligncenter\" src=\"https:&#47;&#47;developers.google.com&#47;accounts&#47;images&#47;sign-in-with-google.png\"
  alt=\"Sign in with Google\" width=\"492\" height=\"108\" &#47;>\r\n\r\nMany web
  UIs come with only basic multi-tenancy. &nbsp;Some web applications come with no
  security at all. &nbsp;A very quick, simple example is the IPython Notebook, a web-based
  python environment, often used as part of the data science process, which has no
  requirement for authentication of users as it is intended as a single-user web application
  and was never meant to be hosted publicly. (I recognize that there are multi-tenant
  Notebook systems out there, but include IPython Notebook as an example)\r\n\r\nEven
  web applications that do allow for authentication, authorization, and accounting
  - the three As of multi-tenancy, will often only support HTTP basic authentication,
  private (ie application-only &#47; application-specific) user databases, or LDAP
  authentication. &nbsp; But the web has moved on. &nbsp;The need to secure web applications,
  RESTful HTTP endpoints, and web services, not to mention the desire for true single
  sign-on, has led to the development of technology and standards such as Security
  Assertion Markup Language (SAML) and OAuth (and ultimately OAuth2).\r\n\r\nThis
  article will focus on OAuth2 and how, along with an open source proxy project, it
  can be used to secure applications which are unable to natively authenticate users
  using OAuth2.\r\n\r\n"
wordpress_id: 14
wordpress_url: https://www.r9labs.org/?p=14
date: '2015-05-18 22:45:15 -0400'
date_gmt: '2015-05-18 22:45:15 -0400'
categories:
- Technology
- Technical
tags:
- nginx
- google
- oauth
- oauth2
- supervisor
- proxy
comments: []
---
<p><img class="aligncenter" src="https:&#47;&#47;developers.google.com&#47;accounts&#47;images&#47;sign-in-with-google.png" alt="Sign in with Google" width="492" height="108" &#47;></p>
<p>Many web UIs come with only basic multi-tenancy. &nbsp;Some web applications come with no security at all. &nbsp;A very quick, simple example is the IPython Notebook, a web-based python environment, often used as part of the data science process, which has no requirement for authentication of users as it is intended as a single-user web application and was never meant to be hosted publicly. (I recognize that there are multi-tenant Notebook systems out there, but include IPython Notebook as an example)</p>
<p>Even web applications that do allow for authentication, authorization, and accounting - the three As of multi-tenancy, will often only support HTTP basic authentication, private (ie application-only &#47; application-specific) user databases, or LDAP authentication. &nbsp; But the web has moved on. &nbsp;The need to secure web applications, RESTful HTTP endpoints, and web services, not to mention the desire for true single sign-on, has led to the development of technology and standards such as Security Assertion Markup Language (SAML) and OAuth (and ultimately OAuth2).</p>
<p>This article will focus on OAuth2 and how, along with an open source proxy project, it can be used to secure applications which are unable to natively authenticate users using OAuth2.</p>
<p><a id="more"></a><a id="more-14"></a></p>
<h2>OAuth2 in the Nutshell<&#47;h2><br />
OAuth2 is the successor to OAuth, originally created in 2006. &nbsp;It allows for the authentication of users via a trusted 3rd party. &nbsp;Today it is not uncommon to log into a website (https:&#47;&#47;bbc.co.uk for instance) using a Facebook or Google account, or some other 3rd party prominent website with a large number of users. When a user chooses to log into a website using their Google account, the website will begin a backoffice authentication process with Google's servers, requesting the user's information. &nbsp;In this instance the user will authenticate themselves with Google's servers and Google's servers will issue a token to the target website, vouching that the user is who they claim to be. &nbsp;This scenario provides added security in that the user no longer has to remember a multitude of usernames and passwords, and can instead authenticate him or herself securely using a single set of credentials.</p>
<p>For a more detailed discussion around the inner workings of OAuth2 I recommend doing a quick <a href="https:&#47;&#47;www.google.co.uk&#47;search?q=oauth2+explained">Web search<&#47;a> or starting with the following link to learn more: <a href="https:&#47;&#47;aaronparecki.com&#47;2012&#47;07&#47;29&#47;2&#47;oauth2-simplified">OAuth2 Simplified<&#47;a>.</p>
<h2>Limited OAuth2 Support<&#47;h2><br />
As has been stated the number of webapps which are OAuth aware is still very low. As a result it may be useful to leverage a proxy to act as the login manager for your application. For the remainder, this article will describe a scenario that places an instance of <a href="http:&#47;&#47;supervisord.org&#47;">Supervisor<&#47;a> behind a proxy which is OAuth aware. When a user attempts to access the Supervisor Web interface the user will first be authenticated via the OAuth proxy before being forwarded to the Supervisor interface. Even if Supervisor does nothing with the authenticated users information a greater level of protection of the Supervisor service is being afforded above a more traditional method such as HTTP Basic authentication.</p>
<h2>Article Focus<&#47;h2><br />
The proxy of choice in this article is the <a href="https:&#47;&#47;github.com&#47;bitly&#47;oauth2_proxy">OAuth2 Proxy<&#47;a> written in <a href="https:&#47;&#47;golang.org&#47;">Go<&#47;a> by the <a href="http:&#47;&#47;word.bitly.com&#47;">Bit.ly team<&#47;a> and hosted on <a href="https:&#47;&#47;github.com&#47;bitly&#47;oauth2_proxy">GitHub<&#47;a>.&nbsp; Along with the configuration of <a href="https:&#47;&#47;www.nginx.com&#47;">NGINX<&#47;a> and Supervisor, this article will focus on the installation and configuration of the OAuth2 Proxy to authenticate users against Google's OAuth service.&nbsp; When configured a user's HTTP &#47; HTTPS request will flow as follows:</p>
<p>User --> NGINX --> OAuth2 Proxy --> Supervisor Web UI</p>
<p>NGINX will listen on the external IP address of a web server and forward requests to an upstream OAuth2 Proxy listening on the loopback address.&nbsp; Subsequently the proxy will forward authenticated requests upstream to the Supervisor which is also running on the loopback address.</p>
<h2>Assumptions<&#47;h2><br />
This article assumes that NGINX&nbsp;and Supervisor are installed as services on your server. It also assumes that Supervisor is running with its Web UI enabled.&nbsp; For more information on how to configure Supervisor to enable its Web UI see this bit of the <a href="http:&#47;&#47;supervisord.org&#47;configuration.html#inet-http-server-section-settings">project's documentation<&#47;a>.</p>
<h2>OAuth2 Proxy Overview<&#47;h2><br />
OAuth2 Proxy is, from the project's Readme,</p>
<blockquote><p>A reverse proxy and static file server that provides authentication using Providers (Google, Github, and others) to validate accounts by email, domain or group.<&#47;blockquote><br />
Bit.ly's OAuth2 Proxy is capable of authenticating against the OAuth services of Google, Github, Azure, Gitlab, LinkedIn, and MyUSA.&nbsp; It works by intercepting (read: proxying) an HTTP request intended for a guarded service, first authenticates the request against a specified OAuth2 identity provider, and then, presuming the authentication is successful, stores the OAuth2 cookie into a browser cookie.&nbsp; The proxy server has matured drastically even in just the last year and has a multitude of features and options that should support most deployment scenarios.</p>
<h2>Preparation<&#47;h2><br />
This article will walk through the following process for deploying and configuring the components previously mentioned.&nbsp; The process will be as follows:</p>
<ol>
<li>Prepare Google's OAuth service for use by the proxy<&#47;li>
<li>Install the OAuth2 Proxy<&#47;li>
<li>Configure the OAuth2 Proxy to forward HTTP requests from an internal port to the desired service port, in this case the Supervisor HTTP server port<&#47;li>
<li>Start the OAuth2 Proxy using Supervisor (this could just as easily be done by installing the proxy as an OS-level service)<&#47;li>
<li>Finally configure NGINX to route messages to the OAuth2 Proxy<&#47;li><br />
<&#47;ol><br />
Note: NGINX is not strictly required in this case, however as this is being used as part of an Internet-facing server I consider NGINX to be far more battle-tested and hardened against malicious Internet traffic when compared to the OAuth2 Proxy or Supervisord.&nbsp; As such NGINX will act as a first, and admittedly very thin, line of defense against malicious traffic.</p>
<h2>Step 1: Prepare Google's OAuth service<&#47;h2><br />
In order for the OAuth proxy to communicate with Google's OAuth service the proxy will require a client ID and client secret.&nbsp; To create the ID and secret a project needs to first be created with Google using the <a href="https:&#47;&#47;console.developers.google.com&#47;project">Google Developer's console<&#47;a>.&nbsp; From the developer's console create a new project, giving it some meaningful name, and next choose the newly created project from the top right project dropdown. In the project's Dashboard choose 'Enable and manage APIs'.&nbsp; Next click 'Credentials' in the left navigation pane followed by clicking the 'OAuth consent screen' tab.&nbsp; Fill in 'Product name show to users' with a meaningful product name, in this case 'Supervisor' will suffice.&nbsp; Click 'Save'.</p>
<p>Now in the center pane select the 'Credentials' tab and open the 'New Credentials' drop down.&nbsp; Choose OAuth client ID and choose 'web application'.&nbsp; Enter an appropriate application name, such as 'supervisor_oauth_proxy' and enter your site's domain name for 'Authorized JavaScript origins', for example https:&#47;&#47;supervisor.example.com.&nbsp; Enter into 'Authorized redirect URIs' the location of the OAuth2 callback, this is typically something like https:&#47;&#47;supervisor.example.com&#47;oauth2&#47;callback.&nbsp; Finally click 'Create' and record the Client ID and Client Secret generated by Google as these will need to be given to the OAuth proxy.</p>
<h2>Step 2: OAuth2 Proxy Installation<&#47;h2><br />
These days the project has pre-compiled binaries available for distribution meaning that it is no longer necessary to learn Go build tools or configure a Go build environment in order to use the Proxy.&nbsp; If you have intentions of contributing to the project or just want to see how the project operates under the hood you're invited to fork or clone the repository as per the typical Github process.&nbsp; However if you just want to be a user of the proxy <a href="https:&#47;&#47;github.com&#47;bitly&#47;oauth2_proxy&#47;releases">obtaining a binary distribution<&#47;a> will do just fine.&nbsp; As such download and extract the binary into an appropriate directory such as &#47;opt&#47;oauth2_proxy.&nbsp; You should now have a binary named 'oauth2_proxy' located at &#47;opt&#47;oauth2_proxy&#47;oauth2_proxy'.</p>
<h2>Step 3: OAuth2 Proxy Configuration<&#47;h2><br />
To hold the configuration of the OAuth2 Proxy and to provide it with a database of approved user email addresses create the directory &#47;opt&#47;oauth2_proxy&#47;etc.&nbsp; This directory will hold the configuration file for the proxy server along with the database of approved email addresses that can log into the Supervisor web UI.</p>
<p>First create the email database, it is a plain text file with an email address per line:</p>
<pre class="lang:default decode:true " title="&#47;opt&#47;oauth2_proxy&#47;etc&#47;authorized_emails">john.doe@example.com<br />
sally.jessie@gmail.com<br />
roger.moore@gmail.com<&#47;pre><br />
Next create a configuration file for the proxy server:</p>
<pre class="lang:default decode:true" title="&#47;opt&#47;oauth2_proxy&#47;etc&#47;supervisor_oauth_proxy.conf"># IP address and port on which the proxy should listen for requests<br />
http_address = "127.0.0.1:4181"<br />
# the IP address, port, and protocol of the Supervisor server<br />
upstreams = [ "http:&#47;&#47;127.0.0.1:9001" ]<br />
request_logging = true<br />
client_id = "insert_google_client_id_here"<br />
client_secret = "insert_google_client_secret_here"<br />
authenticated_emails_file = "&#47;opt&#47;oauth2_proxy&#47;etc&#47;authenticated_emails"<br />
cookie_secret = "secret_passphrase"<&#47;pre><br />
For a complete list of the configuration directives please see the project page for OAuth2 Proxy.</p>
<p>The directives included in the configuration file above are the key parameters to support this particular use case, however you are encouraged to visit the project home page to find out more about the many configurable behaviors of the proxy server.</p>
<p>After authenticating a user via the Google service the OAuth proxy will check whether the authenticated user is authorized to access the service by looking for the user's email address in the email address database identified by the --authenticated-emails-file directive.</p>
<p>To confirm the proxy configuration is valid execute the following command to start the proxy server.&nbsp; Presuming it logs no errors or warnings you can issue a 'Ctrl-C' to stop the proxy server and move on to launching the proxy server from Supervisor.</p>
<pre class="lang:sh decode:true" title="Configuration Validation Command">$ &#47;opt&#47;oauth2_proxy&#47;oauth2_proxy -config &#47;opt&#47;oauth2_proxy&#47;etc&#47;supervisor_oauth2_proxy.conf<&#47;pre></p>
<h2>Step 4: Configure Supervisor<&#47;h2><br />
To have Supervisor launch and manage the OAuth Proxy on startup a program will need to be added to the Supervisor configuration.&nbsp; In order to do this create a configuration file for the OAuth2 Proxy in Supervisor's conf.d directory:</p>
<pre class="lang:default decode:true " title="&#47;etc&#47;supervisor&#47;conf.d&#47;supervisor-oauth.conf">[program:supervisor_oauth]<br />
command=&#47;opt&#47;oauth2_proxy&#47;oauth2_proxy -config=&#47;opt&#47;oauth2_proxy&#47;etc&#47;supervisor_oauth_proxy.conf<br />
numprocs=1<br />
user=nobody<br />
stopsignal=TERM<br />
autostart=true<br />
autorestart=true<br />
redirect_stderr=true<br />
stdout_logfile=auto<&#47;pre><br />
Now reload the Supervisor service and the OAuth2 proxy should be up and listening for requests.&nbsp; The next step is to configure NGINX to forward requests to the proxy server as an upstream destination.</p>
<h2>Step 5: Configure NGINX<&#47;h2><br />
In order to configure NGINX add an upstream server to the NGINX configuration:</p>
<pre class="lang:default decode:true" title="&#47;etc&#47;nginx&#47;conf.d&#47;upstream.conf">...<br />
upstream supervisor-oauth-proxy {<br />
    server 127.0.0.1:4181;<br />
}<&#47;pre><br />
Next add a Supervisor configuration file to NGINX's sites-available directory:</p>
<pre class="lang:default decode:true" title="SupervisorD site configuration for NGINX">server {<br />
server_name supervisor.example.com;<br />
listen 192.168.103.104:80;<br />
return 301 https:&#47;&#47;$host$request_uri;<br />
}</p>
<p>server {<br />
    listen 192.168.103.104:443 ssl;</p>
<p>    ssl_certificate     &#47;etc&#47;ssl&#47;certs&#47;example.com.pub.pem;<br />
    ssl_certificate_key &#47;etc&#47;ssl&#47;private&#47;example.com.priv.pem;<br />
    ssl_protocols       SSLv3 TLSv1 TLSv1.1 TLSv1.2;<br />
    ssl_ciphers         HIGH:!aNULL:!MD5;<br />
    add_header Strict-Transport-Security max-age=1209600;</p>
<p>    root &#47;var&#47;www&#47;html;<br />
    index index.html index.htm;</p>
<p>    server_name super.example.com;</p>
<p>    location &#47; {<br />
        proxy_pass http:&#47;&#47;supervisor-oauth-proxy;<br />
        proxy_set_header Host $host;<br />
        proxy_set_header X-Real-IP $remote_addr;<br />
        proxy_set_header X-Scheme $scheme;<br />
        proxy_connect_timeout 1;<br />
    }<br />
}<&#47;pre><br />
After reloading the NGINX service you should now be able to access http:&#47;&#47;supervisor.example.com and be presented with an option to log in via Google thanks to the OAuth2 proxy.&nbsp; After successfully logging in, presuming your email address is in the approved emails database, you should be forwarded to the Supervisor web interface.</p>
<p>&nbsp;</p>
<p>Enjoy</p>
