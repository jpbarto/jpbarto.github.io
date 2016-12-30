---
layout: single
status: publish
published: false
title: Installing Vagrant on Windows
author:
  display_name: Jason Barto
  login: jpbarto
  email: jason.p.barto@gmail.com
  url: ''
author_login: jpbarto
author_email: jason.p.barto@gmail.com
excerpt: "<h2>Overview<&#47;h2>\r\n<a href=\"https:&#47;&#47;www.vagrantup.com&#47;\">Vagrant<&#47;a>
  is a multi-platform command-line tool for scripting the creation, destruction and
  management of virtual machines. Through the use of plugins Vagrant can interface
  with <a href=\"https:&#47;&#47;www.virtualbox.org&#47;\">VirtualBox<&#47;a>, <a
  href=\"http:&#47;&#47;www.vmware.com&#47;uk&#47;products&#47;vsphere\">VMware vSphere<&#47;a>,
  <a href=\"http:&#47;&#47;www.vmware.com&#47;uk&#47;products&#47;fusion\">VMware
  Fusion<&#47;a>,<a href=\"http:&#47;&#47;aws.amazon.com&#47;\"> Amazon AWS<&#47;a>,
  <a href=\"https:&#47;&#47;azure.microsoft.com&#47;en-gb&#47;\">Windows Azure<&#47;a>,
  and <a href=\"https:&#47;&#47;www.docker.com&#47;\">Docker<&#47;a> containers.\r\n\r\nThe
  following outlines the instructions for installing Vagrant on a Windows desktop
  in order to begin using Vagrant to create virtual machines. This installation guide
  is not intended as an in-depth tutorial on how to use Vagrant but it will provide
  links to documentation where possible. After installing Vagrant and creating a virtual
  machine locally with VirtualBox this guide will progress to modify the Vagrant installation
  to support the creation of virtual machines within VMware vSphere.\r\n\r\nThis guide
  was written using Vagrant 1.8.1. As the development of Vagrant progresses this guide
  may no longer be relevant.\r\n\r\n"
wordpress_id: 190
wordpress_url: https://jasonbarto.com/?p=190
date: '2016-01-23 23:30:54 -0500'
date_gmt: '2016-01-23 23:30:54 -0500'
categories:
- Technology
- Technical
tags:
- vagrant
- windows
- virtualbox
- vsphere
comments: []
---
<h2>Overview<&#47;h2><br />
<a href="https:&#47;&#47;www.vagrantup.com&#47;">Vagrant<&#47;a> is a multi-platform command-line tool for scripting the creation, destruction and management of virtual machines. Through the use of plugins Vagrant can interface with <a href="https:&#47;&#47;www.virtualbox.org&#47;">VirtualBox<&#47;a>, <a href="http:&#47;&#47;www.vmware.com&#47;uk&#47;products&#47;vsphere">VMware vSphere<&#47;a>, <a href="http:&#47;&#47;www.vmware.com&#47;uk&#47;products&#47;fusion">VMware Fusion<&#47;a>,<a href="http:&#47;&#47;aws.amazon.com&#47;"> Amazon AWS<&#47;a>, <a href="https:&#47;&#47;azure.microsoft.com&#47;en-gb&#47;">Windows Azure<&#47;a>, and <a href="https:&#47;&#47;www.docker.com&#47;">Docker<&#47;a> containers.</p>
<p>The following outlines the instructions for installing Vagrant on a Windows desktop in order to begin using Vagrant to create virtual machines. This installation guide is not intended as an in-depth tutorial on how to use Vagrant but it will provide links to documentation where possible. After installing Vagrant and creating a virtual machine locally with VirtualBox this guide will progress to modify the Vagrant installation to support the creation of virtual machines within VMware vSphere.</p>
<p>This guide was written using Vagrant 1.8.1. As the development of Vagrant progresses this guide may no longer be relevant.</p>
<p><a id="more"></a><a id="more-190"></a></p>
<h2>Installation<&#47;h2><br />
To get started you will need to download the following installers:</p>
<ul>
<li><a href="https:&#47;&#47;www.vagrantup.com&#47;downloads.html">Vagrant Windows Installer<&#47;a><&#47;li>
<li><a href="https:&#47;&#47;www.virtualbox.org&#47;wiki&#47;Downloads">Oracle VirtualBox<&#47;a><&#47;li>
<li><a href="https:&#47;&#47;www.microsoft.com&#47;en-gb&#47;download&#47;details.aspx?id=5555">Microsoft Visual C++ 2010 Redistributable<&#47;a><&#47;li>
<li><a href="https:&#47;&#47;git-scm.com&#47;download&#47;win">Git for Windows<&#47;a><&#47;li>
<li><a href="https:&#47;&#47;www.itefix.net&#47;cwrsync">cwRsync Free Edition<&#47;a><&#47;li><br />
<&#47;ul><br />
After downloading the 5 packages above you should have everything needed to install and configure Vagrant for use with both Virtualbox and VMware vSphere. Begin by installing the MS 2010 Redistributable package.</p>
<p>Next install VirtualBox by executing its EXE.</p>
<p>Next install Vagrant by executing the MSI.</p>
<p>Next install Git by executing its EXE.</p>
<p>Finally unzip and place cwRsync 'bin' directory into 'C:\rwSync' on the local Windows machine.</p>
<p>The previous installers will have modified your system's PATH environment variable. In order to avoid issues described here please further modify your environment PATH to place the 'rsync.exe' in 'C:\cwRsync' in the PATH. 'C:\cwRsync' will need to be in the PATH BEFORE the Git PATHs. After a reboot of your system your PATH should look something similar to the following:</p>
<pre class="lang:default decode:true" title="Windows %PATH%">C:\Windows\System32;C:\Windows;C:\Windows\System32\Wbem;C:\Hashicorp\Vagrant\bin;C:\cwRsync;C:\Program Files\Git\cmd;C:\Program Files\Git\Mingw64 \bin;C:\Program Files\Git\usr\bin<&#47;pre></p>
<h2>First Project<&#47;h2><br />
Now that Vagrant is installed to begin using it open a command window (cmd.exe), create a project directory and initialize the project using Vagrant:</p>
<pre class="lang:batch decode:true" title="First Project Initialization">C:\User\home> mkdir vagrant-test<br />
C:\User\home> cd vagrant-test<br />
C:\User\home\vagrant-test> vagrant init<&#47;pre><br />
By calling init Vagrant will have created its project-specific configuration file: Vagrantfile. Open Vagrantfile using your preferred text editor and look over the heavily commented default configuration file. Modify the following configuration line to read:</p>
<pre class="lang:default decode:true " title="First Project Vagrantfile, partial">config.vm.box = 'ubuntu&#47;trusty64'<&#47;pre><br />
Now you are ready to have Vagrant create a virtual machine in VirtualBox that is running the Trusty64 version of Ubuntu linux. Execute vagrant using the following command:</p>
<pre class="lang:batch decode:true" title="First Project Initialization">C:\User\home\vagrant-test> vagrant up<&#47;pre><br />
Vagrant will now read the Vagrantfile project configuration, download the specified ubuntu&#47;trusty64 box from the <a href="https:&#47;&#47;atlas.hashicorp.com&#47;boxes&#47;search">Atlas<&#47;a> and use the downloaded box's image to provision a virtual machine into VirtualBox. Vagrant will write output to the screen to keep you updated as to its progress. Once the box has been created and deployed you can now use Vagrant to SSH into the resulting Linux virtual machine:</p>
<pre class="lang:batch decode:true ">C:\>User\home\vagrant-test> vagrant ssh<&#47;pre><br />
Once you are ready to be rid of the virtual machine ask Vagrant to delete the virtual machine using:</p>
<pre class="lang:batch decode:true ">C:\>User\home\vagrant-test> vagrant destroy<&#47;pre></p>
<h2>On to vSphere<&#47;h2><br />
Vagrant uses plugins called 'providers' to communicate with the various virtual environments such as Virtualbox and vSphere.&nbsp; To communicate with vSphere you will need to download and install the open source <a href="https:&#47;&#47;github.com&#47;nsidc&#47;vagrant-vsphere">vagrant-vsphere<&#47;a> provider plugin.</p>
<p>This plugin relies upon an updated version of the <a href="http:&#47;&#47;nokogiri.org&#47;">Nokogiri<&#47;a> plugin. You will need to modify the Vagrant source code to permit the upgrade of the Nokogiri plugin as well as modify how it constructs a path for use with cwRsync later on. To begin modify the following files:</p>
<pre class="lang:batch decode:true ">C:\Hashicorp\Vagrant\embedded\gems\specifications\vagrant-1.8.1.gemspec<br />
C:\Hashicorp\Vagrant\embedded\gems\gems\vagrant-1.8.1\vagrant.gemspec<&#47;pre><br />
Look for any lines which specify something similar to:</p>
<pre class="lang:ruby decode:true ">s.add_dependency "nokogiri", "= 1.6.3.1"<&#47;pre><br />
and modify it to read:</p>
<pre class="lang:ruby decode:true">s.add_dependency "nokogiri", ">= 1.6.3.1"<&#47;pre><br />
There should be one line changed in vagrant.gemspec and 3 changes in vagrant-1.8.1.gemspec.</p>
<p>Once all changes have been made save the files and return to the command line window. At the command terminal instruct Vagrant to install an updated Nokogiri plugin followed by the Vagrant-Vsphere plugin:</p>
<pre class="lang:batch decode:true ">C:\User\home\vagrant-test> vagrant plugin install nokogiri --plugin-version 1.6.7.1<br />
C:\User\home\vagrant-test> vagrant plugin install vagrant-vsphere<&#47;pre><br />
Vagrant will download the relevant files from the WWW and install them. The Vagrant-VSphere plugin will provide Vagrant with a 'vsphere' provider in addition to the default 'virtualbox' provider for interfacing with VMware vSphere.<br />
You will now be able to modify your Vagrantfile and instruct Vagrant to create virtual machines within your vSphere environment. However when Vagrant goes to synchronize and begin configuring the created VM it will run into problems using RSync to copy files to the remote VM. To correct this further source code changes are required.<br />
Edit <span class="lang:batch decode:true crayon-inline ">C:\Hashicorp\Vagrant\embedded\gems\gems\vagrant-1.8.1\plugins\synced_folders\rsync\helper.rb<&#47;span>&nbsp; using your favorite text editor and find the following lines around line number 80:</p>
<pre class="lang:ruby decode:true  ">"-o ControlMaster=auto "+<br />
"-o ControlPath=#{controlpath} " + "-o ControlPersist=10m "+<&#47;pre><br />
Comment each of the lines out by inserting a '#' before the line, resulting in text which looks similar to:</p>
<pre class="lang:ruby decode:true "># "-o ControlMaster=auto "+<br />
# "-o ControlPath=#{controlpath} " +<br />
# "-o ControlPersist=10m "+<&#47;pre><br />
Also, so that the cwRsync binary can find the local files on the C drive modify the path generation around line 43 to read:</p>
<pre class="lang:ruby decode:true ">if Vagrant::Util::Platform.windows?<br />
hostpath = "&#47;cygdrive"+ Vagrant::Util::Platform.cygwin_path(hostpath) end<&#47;pre><br />
Save the file and you are now ready to configure Vagrant to create virtual machines within vSphere.<br />
Vagrant will not be able to use the previous Linux box with vSphere but will instead need a template to be defined within the target vSphere environment. However a vagrant box will still need to be specified. So install a dummy box for use with vSphere using the following command:</p>
<pre class="lang:batch decode:true ">C:\Users\home\vagrant-test> vagrant box add atlas_shrugged&#47;vsphere_dummy --name vsphere-dummy<&#47;pre><br />
Next edit the Vagrantfile to include configuration parameters for the vSphere environment:</p>
<pre class="lang:default decode:true ">config.vm.provider :vsphere do |vsphere, override|<br />
  override.vm.box = "vsphere-dummy"<br />
  vsphere.host = "vSphere host IP"<br />
  vsphere.insecure = true # don't verify the server SSL&#47;TLS cert<br />
  vsphere.compute_resource_name = "Example SILVER Cluster"<br />
  vsphere.resource_pool_name = "Example LAB 1"<br />
  vsphere.customization_spec_name = "Vagrant_Linux-LAB 1-spec"<br />
  vsphere.template_name = "Templates&#47;Vagrant&#47;Ubuntu-14.04-x86_64-vagrant"<br />
  vsphere.vm_base_path = "Example CLOUD&#47;SILVER_Cluster&#47;VRM&#47;ICT&#47;LAB 1"<br />
  vsphere.vlan = "LAB-1-LAN-1 (1002)"<br />
  vsphere.user = "domain\\your-user-name" # the '\\' double slash is required to 'escape' the slash when the value is passed to vsphere<br />
  vsphere.password = "your-pass-word"<br />
end<&#47;pre><br />
Vagrant now has the information it needs to interface with vSphere on your behalf and create the virtual machine as it did with Virtualbox. To recreate the virtual machine in vSphere use the following vagrant command:</p>
<pre class="lang:batch decode:true ">C:\Users\home\vagrant-test> vagrant up --provider vsphere<&#47;pre><br />
This should connect to vSphere and deploy the virtual machine using the specified VMware template. Once completed Vagrant will copy all files in the vagrant-test directory to the remote server and allow you to connect to the remote server as you did earlier. When finished with the virtual machine you can equally execute 'vagrant destroy' to delete the virtual machine.<br />
Happy vming.</p>
<h2>Reference:<&#47;h2></p>
<ul>
<li><a href="https:&#47;&#47;www.vagrantup.com&#47;docs&#47;">Vagrant documentation<&#47;a><&#47;li>
<li><a href="https:&#47;&#47;adamcod.es&#47;2014&#47;09&#47;23&#47;vagrant-ansible-quickstart-tutorial.html">Vagrant tutorial<&#47;a><&#47;li>
<li><a href="https:&#47;&#47;github.com&#47;nsidc&#47;vagrant-vsphere">Vagrant-VSphere documentation<&#47;a><&#47;li><br />
<&#47;ul></p>
