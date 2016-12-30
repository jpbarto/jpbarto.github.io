---
id: 91
title: Enabling additional transport connectors in RedHat Fuse 6.2
date: 2015-09-13T13:18:12+00:00
author: Jason Barto
layout: single
guid: https://www.r9labs.org/?p=91
permalink: /enabling-additional-transport-connectors-in-redhat-fuse-6-2/
categories:
  - Technical
  - Technology
tags:
  - activemq
  - amqp
  - fuse
  - jboss
  - mqtt
  - stomp
---
At the heart of any enterprise integration or enterprise service bus lies a [message oriented middleware (MOM)](https://en.wikipedia.org/wiki/Message_oriented_middleware "Message oriented middleware"). Whether that MOM is a proprietary solution like [IBM MQ](http://www.ibm.com/software/products/en/ibm-mq "IBM MQ") or an open source solution like [RabbitMQ](https://www.rabbitmq.com/ "RabbitMQ"), every integration product relies upon a messaging bus to enable cross-platform and cross-service communication.  [RedHat&#8217;s JBoss Fuse 6.2](http://www.jboss.org/products/fuse/overview/ "JBoss Fuse") uses an instance of the well known [Apache ActiveMQ](http://activemq.apache.org/ "Apache ActiveMQ") for its communication channel.

ActiveMQ is a battle-proven, robust and well-equipped messaging bus.  It is built using its own [OpenWire](http://activemq.apache.org/openwire.html "OpenWire") protocol, is [JMS](https://en.wikipedia.org/wiki/Java_Message_Service "Java Message Service") compliant, and has native libraries for various languages including .Net, C and Python.  Perhaps most importantly however is its ability to support 3rd party non-proprietary messaging standards such as [AMQP](https://www.amqp.org/ "Advanced Message Queueing Protocol") and [MQTT](http://mqtt.org/ "MQ Transport Telemetry").  With native libraries for other languages ActiveMQ&#8217;s JMS focus will not preclude interoperability with other languages, however any messaging bus using its own proprietary protocol will need its users to prepare for and afford [vendor lock-in](https://en.wikipedia.org/wiki/Vendor_lock-in).  By supporting highly capable standard messaging protocols ActiveMQ enables adopters to side step this caveat.  Integration with ActiveMQ and the wider JBoss Fuse platform is the focus of this article; this article will cover how to enable alternative transports for JBoss Fuse&#8217;s ActiveMQ instance, enabling a polyglot MOM.

By default JBoss Fuse deploys with ActiveMQ running in the root container using the default OpenWire protocol on the default port 61616. If a service or client connects to the Fuse MOM using the ActiveMQ JMS driver, OpenWire on port 61616 is what that driver will be expecting. If you&#8217;re service is written using anything other than Java native communication with ActiveMQ may not be an option.

To enable the Fuse integration platform to permit communication with a wider set of services enabling AMQP, MQTT or [Stomp](https://stomp.github.io/ "Simple Text-Oriented Messaging Protocol") is ideal. All three are standard wire level protocols for messaging and are widely supported by numerous products and libraries, avoiding vendor lock in and enabling the popular [publish / subscribe](http://www.enterpriseintegrationpatterns.com/patterns/messaging/PublishSubscribeChannel.html "Publish Subscribe Channel") [enterprise integration pattern](http://www.enterpriseintegrationpatterns.com/ "Enterprise Integration Patterns").

By the end of this article Fuse will have a modified default configuration of the in-built ActiveMQ instance.  ActiveMQ will still be available on port 61616 using the default settings but will also have the additional transport connectors enabled.

<!--more-->

To begin, like all configuration changes in Fuse, create a profile.  From the Fuse shell issue the following command:

<pre class="lang:sh decode:true ">fabric:profile-create --parent mq-amq mq-polyglot</pre>

This will create a profile named &#8216;polyglot&#8217; in the &#8216;mq&#8217; directory (with the other default MQ profiles).  The profile will adopt all of the settings of its parent, &#8216;mq-amq&#8217; meaning that only additions will be made to the default Fuse capability.

Before assigning this profile to the root container its configuration needs to be tuned.  From the Fuse web console (typically located at [http://localhost:8181](http://127.0.0.1:8181 "JBoss Fuse Web Console")) navigate to &#8216;Profiles&#8217; and find the newly created &#8216;mq-polyglot&#8217; profile, click the &#8216;Details&#8230;&#8217; link for the profile.  Next click the &#8216;Configuration&#8217; button to access the profile&#8217;s configuration sections.  Access the &#8216;ActiveMQ Broker &#8211;> broker&#8217; configuration by clicking the indented &#8216;broker&#8217; button.  The properties file will need to have some values changed and some new values created.  To start with new properties need to be added which will configure the ports on which the new transport connectors will listen for connections.  This article is aiming to enable Stomp, AMQP and MQTT in the default ActiveMQ instance.  To support these connectors 3 new proeprties must be configured.  First click &#8216;Edit&#8217; and then for each property click &#8216;+ Property&#8217; and enter the following key / value pairs before clicking &#8216;Add&#8217; to complete the addition of each new property.

Click &#8216;Edit&#8217; to get started modifying the properties.

Next click &#8216;+ Property&#8217; and for the property key enter &#8216;mqttBindPort&#8217; with a value of &#8216;${port:1883,1893}&#8217;, click &#8216;Add&#8217; to complete the new property.

Next click &#8216;+ Property&#8217; and for the property key enter &#8216;amqpBindPort&#8217; with a value of &#8216;${port:5672,5682}&#8217;, click &#8216;Add&#8217; to complete the new property.

Next click &#8216;+ Property&#8217; and for the property key enter &#8216;stompBindPort&#8217; with a value of &#8216;${port:61613,61614}&#8217;. Now click &#8216;Add&#8217;.

Each of these specifies a starting port and an end port.  Upon instantiation if the transport connector is unable to secure the first port (for example 5672) it will incrementally try 5673, 5674, &#8230; until it reaches the end port.  If after all of the specified allowed ports have been tried and the system is unable to secure a port an exception will be thrown.  If all goes according to plan each transport connector will be listening on the first specified port:

AMQP on 5672

MQTT on 1883

Stomp on 61613

The last thing to do in this properties file is to specify which transport connectors are to be enabled.  Modify the &#8216;Connectors&#8217; property to have the following value:

openwire stomp amqp mqtt

Click &#8216;Save&#8217; to complete the configuration modifications.

The properties file should now have the following contents:

<pre class="lang:default decode:true" title="io.fabric8.mq.fabric.server-broker.properties">stompBindPort = ${port:61612,61614}
standby.pool = default
connectors = openwire mqtt stomp amqp
config.checksum = ${checksum:profile:broker.xml}
bindAddress = 0.0.0.0
config = profile:broker.xml
amqpBindPort = ${port:5672,5682}
group = default
mqttBindPort = ${port:1883,1893}
bindPort = ${port:61616,61626}</pre>

The next step is to create an XML configuration for the ActiveMQ instance. This configuration, a standard [ActiveMQ configuration file](http://activemq.apache.org/xml-configuration.html "ActiveMQ XML Configuration"), will be populated by the configuration properties defined in the last step using placeholders in the XML.  The &#8216;broker.xml&#8217; file will not have been made available through the profile&#8217;s Configuration screen.  The file will have to explicitly be created but first copy the contents of the parent profile&#8217;s &#8216;broker.xml&#8217;.

Select the &#8216;Profiles&#8217; tab and find the &#8216;mq-amq&#8217; profile.  Open the &#8216;Details&#8230;&#8217; page for the profile and scroll down to find the &#8216;broker.xml&#8217; configuration file.  Click on the file to view its contents and copy the contents to paste later into the new &#8216;mq-polyglot&#8217; &#8216;broker.xml&#8217;.

Navigate back to the &#8216;mq-polyglot&#8217; details profile screen.  From the profile configuration page create the requisite &#8216;broker.xml&#8217; configuration file.  Click the &#8216;+ Create&#8217; link toward the upper portion of the screen.  This should open a dialog asking for information about the type of file that should be created.  Select &#8216;XML Document&#8217;, enter &#8216;broker.xml&#8217; for a name and then click &#8216;Create&#8217;.  This will present a blank XML document editor into which should be pasted the contents of the &#8216;mq-amq&#8217; &#8216;broker.xml&#8217; document.

With the original &#8216;broker.xml&#8217; copied the configuration XML for the new transport connectors must be entered.  The following links provide information about the various network protocols supported by ActiveMQ.

[http://activemq.apache.org/protocols.html](http://activemq.apache.org/protocols.html "ActiveMQ Protocols")

[http://activemq.apache.org/amqp.html](http://activemq.apache.org/amqp.html "ActiveMQ AMQP Configuration")

[http://activemq.apache.org/mqtt.html](http://activemq.apache.org/mqtt.html "ActiveMQ MQTT Configuration")

[http://activemq.apache.org/stomp.html](http://activemq.apache.org/stomp.html "ActiveMQ Stomp Configuration")

For immediate purposes enable Stomp, AMQP and MQTT by adding the following to the &#8216;transportConnectors&#8217; section:

<pre class="lang:xhtml decode:true">&lt;transportConnectors&gt;
&lt;transportConnector name="amqp" uri="amqp://${bindAddress}:${amqpBindPort}"/&gt;
&lt;transportConnector name="mqtt" uri="mqtt://${bindAddress}:${mqttBindPort}"/&gt;
&lt;transportConnector name="stomp" uri="stomp://${bindAddress}:${stompBindPort}"/&gt;
&lt;/transportConnectors&gt;</pre>

Click &#8216;Save&#8217; after the modifications have been completed and return to the Profile Details page.

The &#8216;broker.xml&#8217; file should now look similar to the following:

<pre class="lang:default decode:true  " title="broker.xml">&lt;?xml version="1.0" encoding="UTF-8"?&gt;
&lt;!--

Copyright 2005-2014 Red Hat, Inc.

Red Hat licenses this file to you under the Apache License, version
2.0 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing
permissions and limitations under the License.

--&gt;
&lt;beans
xmlns="http://www.springframework.org/schema/beans"
xmlns:amq="http://activemq.apache.org/schema/core"
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xsi:schemaLocation="http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
http://activemq.apache.org/schema/core http://activemq.apache.org/schema/core/activemq-core.xsd"&gt;

&lt;!-- Allows us to use system properties and fabric as variables in this configuration file --&gt;
&lt;bean class="org.springframework.beans.factory.config.PropertyPlaceholderConfigurer"&gt;
&lt;property name="properties"&gt;
&lt;bean class="io.fabric8.mq.fabric.ConfigurationProperties"/&gt;
&lt;/property&gt;
&lt;/bean&gt;

&lt;broker xmlns="http://activemq.apache.org/schema/core" brokerName="${broker-name}" dataDirectory="${data}" start="false" restartAllowed="false"&gt;

&lt;destinationPolicy&gt;
&lt;policyMap&gt;
&lt;policyEntries&gt;
&lt;policyEntry topic="&gt;" producerFlowControl="true"&gt;
&lt;pendingMessageLimitStrategy&gt;
&lt;constantPendingMessageLimitStrategy limit="1000"/&gt;
&lt;/pendingMessageLimitStrategy&gt;
&lt;/policyEntry&gt;
&lt;policyEntry queue="&gt;" producerFlowControl="true" memoryLimit="1mb"&gt;
&lt;/policyEntry&gt;
&lt;/policyEntries&gt;
&lt;/policyMap&gt;
&lt;/destinationPolicy&gt;

&lt;managementContext&gt;
&lt;managementContext createConnector="false"/&gt;
&lt;/managementContext&gt;

&lt;persistenceAdapter&gt;
&lt;kahaDB directory="${data}/kahadb"/&gt;
&lt;/persistenceAdapter&gt;

&lt;plugins&gt;
&lt;jaasAuthenticationPlugin configuration="karaf" /&gt;
&lt;/plugins&gt;

&lt;systemUsage&gt;
&lt;systemUsage&gt;
&lt;memoryUsage&gt;
&lt;memoryUsage percentOfJvmHeap="70"/&gt;
&lt;/memoryUsage&gt;
&lt;storeUsage&gt;
&lt;storeUsage limit="100 gb"/&gt;
&lt;/storeUsage&gt;
&lt;tempUsage&gt;
&lt;tempUsage limit="50 gb"/&gt;
&lt;/tempUsage&gt;
&lt;/systemUsage&gt;
&lt;/systemUsage&gt;

&lt;transportConnectors&gt;
&lt;transportConnector name="openwire" uri="tcp://${bindAddress}:${bindPort}"/&gt; 
&lt;transportConnector name="amqp" uri="amqp://${bindAddress}:${amqpBindPort}"/&gt;
&lt;transportConnector name="mqtt" uri="mqtt://${bindAddress}:${mqttBindPort}"/&gt;
&lt;transportConnector name="stomp" uri="stomp://${bindAddress}:${stompBindPort}"/&gt;
&lt;/transportConnectors&gt;
&lt;/broker&gt;
&lt;/beans&gt;</pre>

The last step is to assign the new &#8216;mq-polyglot&#8217; profile to a container, overriding the old &#8216;mq-amq&#8217; profile.  From the profile details page for &#8216;mq-polyglot&#8217; click &#8216;Assign&#8217; and select the &#8216;root&#8217; container, clicking the &#8216;Assign&#8217; button again.  This will modify the exiting ActiveMQ deployment based upon the newly created &#8216;broker.xml&#8217; file and using the properties file defined within the &#8216;mq-polyglot&#8217; profile.

The end result should be a JBoss Fuse instance which is now able to communicate as an AMQP broker, an MQTT broker, a Stomp broker AND a JMS-compliant ActiveMQ OpenWire broker.  A messaging backbone primed for microservices.
