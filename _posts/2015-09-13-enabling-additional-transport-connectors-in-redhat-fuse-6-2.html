---
layout: single
status: publish
published: true
title: Enabling additional transport connectors in RedHat Fuse 6.2
author:
  display_name: Jason Barto
  login: jpbarto
  email: jason.p.barto@gmail.com
  url: ''
author_login: jpbarto
author_email: jason.p.barto@gmail.com
excerpt: "At the heart of any enterprise integration or enterprise service bus lies
  a <a title=\"Message oriented middleware\" href=\"https:&#47;&#47;en.wikipedia.org&#47;wiki&#47;Message_oriented_middleware\">message
  oriented middleware (MOM)<&#47;a>. Whether that MOM is a proprietary solution like
  <a title=\"IBM MQ\" href=\"http:&#47;&#47;www.ibm.com&#47;software&#47;products&#47;en&#47;ibm-mq\">IBM
  MQ<&#47;a> or an open source solution like <a title=\"RabbitMQ\" href=\"https:&#47;&#47;www.rabbitmq.com&#47;\">RabbitMQ<&#47;a>,
  every integration product relies upon a messaging bus to enable cross-platform and
  cross-service communication. &nbsp;<a title=\"JBoss Fuse\" href=\"http:&#47;&#47;www.jboss.org&#47;products&#47;fuse&#47;overview&#47;\">RedHat's
  JBoss Fuse 6.2<&#47;a> uses an instance of the well known <a title=\"Apache ActiveMQ\"
  href=\"http:&#47;&#47;activemq.apache.org&#47;\">Apache ActiveMQ<&#47;a> for its
  communication channel.\r\n\r\nActiveMQ is a battle-proven, robust and well-equipped
  messaging bus. &nbsp;It is built using its own <a title=\"OpenWire\" href=\"http:&#47;&#47;activemq.apache.org&#47;openwire.html\">OpenWire<&#47;a>
  protocol, is <a title=\"Java Message Service\" href=\"https:&#47;&#47;en.wikipedia.org&#47;wiki&#47;Java_Message_Service\">JMS<&#47;a>
  compliant, and has native libraries for various languages including .Net, C and
  Python. &nbsp;Perhaps most importantly however is its ability to support 3rd party
  non-proprietary messaging standards such as <a title=\"Advanced Message Queueing
  Protocol\" href=\"https:&#47;&#47;www.amqp.org&#47;\">AMQP<&#47;a> and <a title=\"MQ
  Transport Telemetry\" href=\"http:&#47;&#47;mqtt.org&#47;\">MQTT<&#47;a>. &nbsp;With
  native libraries for other languages ActiveMQ's JMS focus will not preclude interoperability
  with other languages, however any messaging bus using its own proprietary protocol
  will need its users to prepare for and afford <a href=\"https:&#47;&#47;en.wikipedia.org&#47;wiki&#47;Vendor_lock-in\">vendor
  lock-in<&#47;a>. &nbsp;By supporting highly capable standard messaging protocols
  ActiveMQ enables adopters to side step this caveat. &nbsp;Integration with ActiveMQ
  and the wider JBoss Fuse platform is the focus of this article; this article will
  cover how to enable alternative transports for JBoss Fuse's ActiveMQ instance, enabling
  a polyglot MOM.\r\n\r\nBy default JBoss Fuse&nbsp;deploys&nbsp;with ActiveMQ&nbsp;running
  in the root container using the default OpenWire&nbsp;protocol on the default port
  61616. If a service or client connects to the Fuse MOM using the ActiveMQ JMS driver,
  OpenWire on port 61616 is what that driver will be expecting.&nbsp;If you're service
  is written using anything other than Java native communication&nbsp;with ActiveMQ&nbsp;may
  not be an option.\r\n\r\nTo enable the Fuse&nbsp;integration platform to permit
  communication with a wider set of services enabling AMQP, MQTT&nbsp;or <a title=\"Simple
  Text-Oriented Messaging Protocol\" href=\"https:&#47;&#47;stomp.github.io&#47;\">Stomp<&#47;a>&nbsp;is
  ideal. All three are standard wire level protocols for messaging and are widely
  supported by numerous products and libraries, avoiding vendor lock in and enabling
  the popular <a title=\"Publish Subscribe Channel\" href=\"http:&#47;&#47;www.enterpriseintegrationpatterns.com&#47;patterns&#47;messaging&#47;PublishSubscribeChannel.html\">publish
  &#47; subscribe<&#47;a><a title=\"Enterprise Integration Patterns\" href=\"http:&#47;&#47;www.enterpriseintegrationpatterns.com&#47;\">
  enterprise integration pattern<&#47;a>.\r\n\r\nBy the end of this article Fuse will
  have a modified default configuration of the in-built ActiveMQ instance. &nbsp;ActiveMQ
  will still be available on port 61616 using the default settings but will also have
  the additional transport connectors enabled.\r\n\r\n"
wordpress_id: 91
wordpress_url: https://www.r9labs.org/?p=91
date: '2015-09-13 13:18:12 -0400'
date_gmt: '2015-09-13 13:18:12 -0400'
categories:
- Technology
- Technical
tags:
- activemq
- jboss
- amqp
- mqtt
- stomp
- fuse
comments: []
---
<p>At the heart of any enterprise integration or enterprise service bus lies a <a title="Message oriented middleware" href="https:&#47;&#47;en.wikipedia.org&#47;wiki&#47;Message_oriented_middleware">message oriented middleware (MOM)<&#47;a>. Whether that MOM is a proprietary solution like <a title="IBM MQ" href="http:&#47;&#47;www.ibm.com&#47;software&#47;products&#47;en&#47;ibm-mq">IBM MQ<&#47;a> or an open source solution like <a title="RabbitMQ" href="https:&#47;&#47;www.rabbitmq.com&#47;">RabbitMQ<&#47;a>, every integration product relies upon a messaging bus to enable cross-platform and cross-service communication. &nbsp;<a title="JBoss Fuse" href="http:&#47;&#47;www.jboss.org&#47;products&#47;fuse&#47;overview&#47;">RedHat's JBoss Fuse 6.2<&#47;a> uses an instance of the well known <a title="Apache ActiveMQ" href="http:&#47;&#47;activemq.apache.org&#47;">Apache ActiveMQ<&#47;a> for its communication channel.</p>
<p>ActiveMQ is a battle-proven, robust and well-equipped messaging bus. &nbsp;It is built using its own <a title="OpenWire" href="http:&#47;&#47;activemq.apache.org&#47;openwire.html">OpenWire<&#47;a> protocol, is <a title="Java Message Service" href="https:&#47;&#47;en.wikipedia.org&#47;wiki&#47;Java_Message_Service">JMS<&#47;a> compliant, and has native libraries for various languages including .Net, C and Python. &nbsp;Perhaps most importantly however is its ability to support 3rd party non-proprietary messaging standards such as <a title="Advanced Message Queueing Protocol" href="https:&#47;&#47;www.amqp.org&#47;">AMQP<&#47;a> and <a title="MQ Transport Telemetry" href="http:&#47;&#47;mqtt.org&#47;">MQTT<&#47;a>. &nbsp;With native libraries for other languages ActiveMQ's JMS focus will not preclude interoperability with other languages, however any messaging bus using its own proprietary protocol will need its users to prepare for and afford <a href="https:&#47;&#47;en.wikipedia.org&#47;wiki&#47;Vendor_lock-in">vendor lock-in<&#47;a>. &nbsp;By supporting highly capable standard messaging protocols ActiveMQ enables adopters to side step this caveat. &nbsp;Integration with ActiveMQ and the wider JBoss Fuse platform is the focus of this article; this article will cover how to enable alternative transports for JBoss Fuse's ActiveMQ instance, enabling a polyglot MOM.</p>
<p>By default JBoss Fuse&nbsp;deploys&nbsp;with ActiveMQ&nbsp;running in the root container using the default OpenWire&nbsp;protocol on the default port 61616. If a service or client connects to the Fuse MOM using the ActiveMQ JMS driver, OpenWire on port 61616 is what that driver will be expecting.&nbsp;If you're service is written using anything other than Java native communication&nbsp;with ActiveMQ&nbsp;may not be an option.</p>
<p>To enable the Fuse&nbsp;integration platform to permit communication with a wider set of services enabling AMQP, MQTT&nbsp;or <a title="Simple Text-Oriented Messaging Protocol" href="https:&#47;&#47;stomp.github.io&#47;">Stomp<&#47;a>&nbsp;is ideal. All three are standard wire level protocols for messaging and are widely supported by numerous products and libraries, avoiding vendor lock in and enabling the popular <a title="Publish Subscribe Channel" href="http:&#47;&#47;www.enterpriseintegrationpatterns.com&#47;patterns&#47;messaging&#47;PublishSubscribeChannel.html">publish &#47; subscribe<&#47;a><a title="Enterprise Integration Patterns" href="http:&#47;&#47;www.enterpriseintegrationpatterns.com&#47;"> enterprise integration pattern<&#47;a>.</p>
<p>By the end of this article Fuse will have a modified default configuration of the in-built ActiveMQ instance. &nbsp;ActiveMQ will still be available on port 61616 using the default settings but will also have the additional transport connectors enabled.</p>
<p><a id="more"></a><a id="more-91"></a></p>
<p>To begin, like all configuration changes in Fuse, create a profile. &nbsp;From the Fuse shell issue the following command:</p>
<pre class="lang:sh decode:true ">fabric:profile-create --parent mq-amq mq-polyglot<&#47;pre><br />
This will create a profile named 'polyglot' in the 'mq' directory (with the other default MQ profiles). &nbsp;The profile will adopt all of the settings of its parent, 'mq-amq' meaning that only additions will be made&nbsp;to the default Fuse capability.</p>
<p>Before assigning this profile to the root container its configuration needs to be tuned. &nbsp;From the Fuse web console (typically located at <a title="JBoss Fuse Web Console" href="http:&#47;&#47;127.0.0.1:8181">http:&#47;&#47;localhost:8181<&#47;a>) navigate to 'Profiles' and find the newly created 'mq-polyglot' profile, click the 'Details...' link for the profile. &nbsp;Next click the 'Configuration' button to access the profile's configuration sections. &nbsp;Access the 'ActiveMQ Broker --> broker' configuration by clicking the indented 'broker' button. &nbsp;The properties file will need to have some values changed and some new values created. &nbsp;To start with new properties need to be added which will configure the ports on which the new transport connectors will listen for connections. &nbsp;This article is aiming to enable Stomp, AMQP and MQTT in the default ActiveMQ instance. &nbsp;To support these connectors 3 new proeprties must be configured. &nbsp;First click 'Edit' and then for each property click '+ Property' and enter the following key &#47; value pairs before clicking 'Add' to complete the addition of each new property.</p>
<p>Click 'Edit' to get started modifying the properties.</p>
<p>Next click '+ Property' and for the property key enter 'mqttBindPort' with a value of '${port:1883,1893}', click 'Add' to complete the new property.</p>
<p>Next click '+ Property' and for the property key enter 'amqpBindPort' with a value of '${port:5672,5682}', click 'Add' to complete the new property.</p>
<p>Next&nbsp;click '+ Property' and for the property key enter 'stompBindPort' with a value of '${port:61613,61614}'. Now click 'Add'.</p>
<p>Each of these specifies a starting port and an end port. &nbsp;Upon instantiation if the transport connector is unable to secure the first port (for example 5672) it will incrementally try 5673, 5674, ... until it reaches the end port. &nbsp;If after all of the specified allowed ports have been tried and the system is unable to secure a port an exception will be thrown. &nbsp;If all goes according to plan each transport connector will be listening on the first specified port:</p>
<p>AMQP on 5672</p>
<p>MQTT on 1883</p>
<p>Stomp on 61613</p>
<p>The last thing to do in this properties file is to specify which transport connectors are to be enabled. &nbsp;Modify the 'Connectors' property to have the following value:</p>
<p>openwire stomp amqp mqtt</p>
<p>Click 'Save' to complete the configuration modifications.</p>
<p>The properties file should now have the following contents:</p>
<pre class="lang:default decode:true" title="io.fabric8.mq.fabric.server-broker.properties">stompBindPort = ${port:61612,61614}<br />
standby.pool = default<br />
connectors = openwire mqtt stomp amqp<br />
config.checksum = ${checksum:profile:broker.xml}<br />
bindAddress = 0.0.0.0<br />
config = profile:broker.xml<br />
amqpBindPort = ${port:5672,5682}<br />
group = default<br />
mqttBindPort = ${port:1883,1893}<br />
bindPort = ${port:61616,61626}<&#47;pre><br />
The next step is to create an XML configuration for the ActiveMQ instance. This configuration, a standard <a title="ActiveMQ XML Configuration" href="http:&#47;&#47;activemq.apache.org&#47;xml-configuration.html">ActiveMQ&nbsp;configuration file<&#47;a>, will be populated by the configuration properties defined in the last step using placeholders in the XML. &nbsp;The 'broker.xml' file will not have been made available through the profile's Configuration screen. &nbsp;The file will have to explicitly be created but first copy the contents of the parent profile's 'broker.xml'.</p>
<p>Select the 'Profiles' tab and find the 'mq-amq' profile. &nbsp;Open the 'Details...' page for the profile and scroll down to find the 'broker.xml' configuration file. &nbsp;Click on the file to view its contents and copy the contents to paste later into the new 'mq-polyglot' 'broker.xml'.</p>
<p>Navigate back to the 'mq-polyglot' details profile screen. &nbsp;From the profile configuration page create the requisite 'broker.xml' configuration file. &nbsp;Click the '+ Create' link toward the upper portion of the screen. &nbsp;This should open a dialog asking for information about the type of file that should be created. &nbsp;Select 'XML Document', enter 'broker.xml' for a name and then click 'Create'. &nbsp;This will present a blank XML document editor into which should be pasted the contents of the 'mq-amq' 'broker.xml' document.</p>
<p>With the original 'broker.xml' copied the configuration XML for the new transport connectors must be entered. &nbsp;The following links provide information about the various network protocols supported by ActiveMQ.</p>
<p><a title="ActiveMQ Protocols" href="http:&#47;&#47;activemq.apache.org&#47;protocols.html">http:&#47;&#47;activemq.apache.org&#47;protocols.html<&#47;a></p>
<p><a title="ActiveMQ AMQP Configuration" href="http:&#47;&#47;activemq.apache.org&#47;amqp.html">http:&#47;&#47;activemq.apache.org&#47;amqp.html<&#47;a></p>
<p><a title="ActiveMQ MQTT Configuration" href="http:&#47;&#47;activemq.apache.org&#47;mqtt.html">http:&#47;&#47;activemq.apache.org&#47;mqtt.html<&#47;a></p>
<p><a title="ActiveMQ Stomp Configuration" href="http:&#47;&#47;activemq.apache.org&#47;stomp.html">http:&#47;&#47;activemq.apache.org&#47;stomp.html<&#47;a></p>
<p>For immediate purposes enable Stomp, AMQP and MQTT by adding the following to the 'transportConnectors' section:</p>
<pre class="lang:xhtml decode:true">
<transportConnectors>
<transportConnector name="amqp" uri="amqp:&#47;&#47;${bindAddress}:${amqpBindPort}"&#47;>
<transportConnector name="mqtt" uri="mqtt:&#47;&#47;${bindAddress}:${mqttBindPort}"&#47;>
<transportConnector name="stomp" uri="stomp:&#47;&#47;${bindAddress}:${stompBindPort}"&#47;>
<&#47;transportConnectors><&#47;pre><br />
Click 'Save' after the modifications have been completed and return to the Profile Details page.</p>
<p>The 'broker.xml' file should now look similar to the following:</p>
<pre class="lang:default decode:true  " title="broker.xml"><?xml version="1.0" encoding="UTF-8"?><br />
<!--</p>
<p>Copyright 2005-2014 Red Hat, Inc.</p>
<p>Red Hat licenses this file to you under the Apache License, version<br />
2.0 (the "License"); you may not use this file except in compliance<br />
with the License. You may obtain a copy of the License at</p>
<p>http:&#47;&#47;www.apache.org&#47;licenses&#47;LICENSE-2.0</p>
<p>Unless required by applicable law or agreed to in writing, software<br />
distributed under the License is distributed on an "AS IS" BASIS,<br />
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or<br />
implied. See the License for the specific language governing<br />
permissions and limitations under the License.</p>
<p>--><br />
<beans<br />
xmlns="http:&#47;&#47;www.springframework.org&#47;schema&#47;beans"<br />
xmlns:amq="http:&#47;&#47;activemq.apache.org&#47;schema&#47;core"<br />
xmlns:xsi="http:&#47;&#47;www.w3.org&#47;2001&#47;XMLSchema-instance"<br />
xsi:schemaLocation="http:&#47;&#47;www.springframework.org&#47;schema&#47;beans http:&#47;&#47;www.springframework.org&#47;schema&#47;beans&#47;spring-beans.xsd<br />
http:&#47;&#47;activemq.apache.org&#47;schema&#47;core http:&#47;&#47;activemq.apache.org&#47;schema&#47;core&#47;activemq-core.xsd"></p>
<p><!-- Allows us to use system properties and fabric as variables in this configuration file --><br />
<bean class="org.springframework.beans.factory.config.PropertyPlaceholderConfigurer"></p>
<property name="properties">
<bean class="io.fabric8.mq.fabric.ConfigurationProperties"&#47;><br />
<&#47;property><br />
<&#47;bean></p>
<p><broker xmlns="http:&#47;&#47;activemq.apache.org&#47;schema&#47;core" brokerName="${broker-name}" dataDirectory="${data}" start="false" restartAllowed="false"></p>
<p><destinationPolicy></p>
<policyMap>
<policyEntries>
<policyEntry topic=">" producerFlowControl="true"></p>
<pendingMessageLimitStrategy>
<constantPendingMessageLimitStrategy limit="1000"&#47;><br />
<&#47;pendingMessageLimitStrategy><br />
<&#47;policyEntry></p>
<policyEntry queue=">" producerFlowControl="true" memoryLimit="1mb"><br />
<&#47;policyEntry><br />
<&#47;policyEntries><br />
<&#47;policyMap><br />
<&#47;destinationPolicy></p>
<p><managementContext><br />
<managementContext createConnector="false"&#47;><br />
<&#47;managementContext></p>
<persistenceAdapter>
<kahaDB directory="${data}&#47;kahadb"&#47;><br />
<&#47;persistenceAdapter></p>
<plugins>
<jaasAuthenticationPlugin configuration="karaf" &#47;><br />
<&#47;plugins></p>
<p><systemUsage><br />
<systemUsage><br />
<memoryUsage><br />
<memoryUsage percentOfJvmHeap="70"&#47;><br />
<&#47;memoryUsage><br />
<storeUsage><br />
<storeUsage limit="100 gb"&#47;><br />
<&#47;storeUsage><br />
<tempUsage><br />
<tempUsage limit="50 gb"&#47;><br />
<&#47;tempUsage><br />
<&#47;systemUsage><br />
<&#47;systemUsage></p>
<transportConnectors>
<transportConnector name="openwire" uri="tcp:&#47;&#47;${bindAddress}:${bindPort}"&#47;>
<transportConnector name="amqp" uri="amqp:&#47;&#47;${bindAddress}:${amqpBindPort}"&#47;>
<transportConnector name="mqtt" uri="mqtt:&#47;&#47;${bindAddress}:${mqttBindPort}"&#47;>
<transportConnector name="stomp" uri="stomp:&#47;&#47;${bindAddress}:${stompBindPort}"&#47;>
<&#47;transportConnectors><br />
<&#47;broker><br />
<&#47;beans><&#47;pre><br />
The last step is to assign the new 'mq-polyglot' profile to a container, overriding the old 'mq-amq' profile. &nbsp;From the profile details page for 'mq-polyglot' click 'Assign' and select the 'root' container, clicking the 'Assign' button again. &nbsp;This will modify the exiting ActiveMQ deployment based upon the newly created 'broker.xml' file and using the properties file defined within the 'mq-polyglot' profile.</p>
<p>The end result should be a JBoss Fuse instance which is now able to communicate as an AMQP broker, an MQTT broker, a Stomp broker AND a JMS-compliant ActiveMQ OpenWire broker. &nbsp;A messaging backbone primed for microservices.</p>
