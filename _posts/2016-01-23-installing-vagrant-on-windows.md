---
title: Installing Vagrant on Windows
layout: single
published: true
categories:
  - Technical
  - Technology
tags:
  - vagrant
  - virtualbox
  - vsphere
  - windows
---
## Overview

[Vagrant](https://www.vagrantup.com/) is a multi-platform command-line tool for scripting the creation, destruction and management of virtual machines. Through the use of plugins Vagrant can interface with [VirtualBox](https://www.virtualbox.org/), [VMware vSphere](http://www.vmware.com/uk/products/vsphere), [VMware Fusion](http://www.vmware.com/uk/products/fusion), [Amazon AWS](http://aws.amazon.com/), [Windows Azure](https://azure.microsoft.com/en-gb/), and [Docker](https://www.docker.com/) containers.

The following outlines the instructions for installing Vagrant on a Windows desktop in order to begin using Vagrant to create virtual machines. This installation guide is not intended as an in-depth tutorial on how to use Vagrant but it will provide links to documentation where possible. After installing Vagrant and creating a virtual machine locally with VirtualBox this guide will progress to modify the Vagrant installation to support the creation of virtual machines within VMware vSphere.

This guide was written using Vagrant 1.8.1. As the development of Vagrant progresses this guide may no longer be relevant.

<!--more-->

## Installation

To get started you will need to download the following installers:

  * [Vagrant Windows Installer](https://www.vagrantup.com/downloads.html)
  * [Oracle VirtualBox](https://www.virtualbox.org/wiki/Downloads)
  * [Microsoft Visual C++ 2010 Redistributable](https://www.microsoft.com/en-gb/download/details.aspx?id=5555)
  * [Git for Windows](https://git-scm.com/download/win)
  * [cwRsync Free Edition](https://www.itefix.net/cwrsync)

After downloading the 5 packages above you should have everything needed to install and configure Vagrant for use with both Virtualbox and VMware vSphere. Begin by installing the MS 2010 Redistributable package.

Next install VirtualBox by executing its EXE.

Next install Vagrant by executing the MSI.

Next install Git by executing its EXE.

Finally unzip and place cwRsync &#8216;bin&#8217; directory into &#8216;C:\rwSync&#8217; on the local Windows machine.

The previous installers will have modified your system&#8217;s PATH environment variable. In order to avoid issues described here please further modify your environment PATH to place the &#8216;rsync.exe&#8217; in &#8216;C:\cwRsync&#8217; in the PATH. &#8216;C:\cwRsync&#8217; will need to be in the PATH BEFORE the Git PATHs. After a reboot of your system your PATH should look something similar to the following:

<pre class="lang:default decode:true" title="Windows %PATH%">C:\Windows\System32;C:\Windows;C:\Windows\System32\Wbem;C:\Hashicorp\Vagrant\bin;C:\cwRsync;C:\Program Files\Git\cmd;C:\Program Files\Git\Mingw64 \bin;C:\Program Files\Git\usr\bin</pre>

## First Project

Now that Vagrant is installed to begin using it open a command window (cmd.exe), create a project directory and initialize the project using Vagrant:

<pre class="lang:batch decode:true" title="First Project Initialization">C:\User\home&gt; mkdir vagrant-test
C:\User\home&gt; cd vagrant-test
C:\User\home\vagrant-test&gt; vagrant init</pre>

By calling init Vagrant will have created its project-specific configuration file: Vagrantfile. Open Vagrantfile using your preferred text editor and look over the heavily commented default configuration file. Modify the following configuration line to read:

<pre class="lang:default decode:true " title="First Project Vagrantfile, partial">config.vm.box = 'ubuntu/trusty64'</pre>

Now you are ready to have Vagrant create a virtual machine in VirtualBox that is running the Trusty64 version of Ubuntu linux. Execute vagrant using the following command:

<pre class="lang:batch decode:true" title="First Project Initialization">C:\User\home\vagrant-test&gt; vagrant up</pre>

Vagrant will now read the Vagrantfile project configuration, download the specified ubuntu/trusty64 box from the [Atlas](https://atlas.hashicorp.com/boxes/search) and use the downloaded box&#8217;s image to provision a virtual machine into VirtualBox. Vagrant will write output to the screen to keep you updated as to its progress. Once the box has been created and deployed you can now use Vagrant to SSH into the resulting Linux virtual machine:

<pre class="lang:batch decode:true ">C:\&gt;User\home\vagrant-test&gt; vagrant ssh</pre>

Once you are ready to be rid of the virtual machine ask Vagrant to delete the virtual machine using:

<pre class="lang:batch decode:true ">C:\&gt;User\home\vagrant-test&gt; vagrant destroy</pre>

## On to vSphere

Vagrant uses plugins called &#8216;providers&#8217; to communicate with the various virtual environments such as Virtualbox and vSphere.  To communicate with vSphere you will need to download and install the open source [vagrant-vsphere](https://github.com/nsidc/vagrant-vsphere) provider plugin.

This plugin relies upon an updated version of the [Nokogiri](http://nokogiri.org/) plugin. You will need to modify the Vagrant source code to permit the upgrade of the Nokogiri plugin as well as modify how it constructs a path for use with cwRsync later on. To begin modify the following files:

<pre class="lang:batch decode:true ">C:\Hashicorp\Vagrant\embedded\gems\specifications\vagrant-1.8.1.gemspec
C:\Hashicorp\Vagrant\embedded\gems\gems\vagrant-1.8.1\vagrant.gemspec</pre>

Look for any lines which specify something similar to:

<pre class="lang:ruby decode:true ">s.add_dependency "nokogiri", "= 1.6.3.1"</pre>

and modify it to read:

<pre class="lang:ruby decode:true">s.add_dependency "nokogiri", "&gt;= 1.6.3.1"</pre>

There should be one line changed in vagrant.gemspec and 3 changes in vagrant-1.8.1.gemspec.

Once all changes have been made save the files and return to the command line window. At the command terminal instruct Vagrant to install an updated Nokogiri plugin followed by the Vagrant-Vsphere plugin:

<pre class="lang:batch decode:true ">C:\User\home\vagrant-test&gt; vagrant plugin install nokogiri --plugin-version 1.6.7.1
C:\User\home\vagrant-test&gt; vagrant plugin install vagrant-vsphere</pre>

Vagrant will download the relevant files from the WWW and install them. The Vagrant-VSphere plugin will provide Vagrant with a &#8216;vsphere&#8217; provider in addition to the default &#8216;virtualbox&#8217; provider for interfacing with VMware vSphere.
  
You will now be able to modify your Vagrantfile and instruct Vagrant to create virtual machines within your vSphere environment. However when Vagrant goes to synchronize and begin configuring the created VM it will run into problems using RSync to copy files to the remote VM. To correct this further source code changes are required.
  
Edit <span class="lang:batch decode:true crayon-inline ">C:\Hashicorp\Vagrant\embedded\gems\gems\vagrant-1.8.1\plugins\synced_folders\rsync\helper.rb</span>  using your favorite text editor and find the following lines around line number 80:

<pre class="lang:ruby decode:true  ">"-o ControlMaster=auto "+
"-o ControlPath=#{controlpath} " + "-o ControlPersist=10m "+</pre>

Comment each of the lines out by inserting a &#8216;#&#8217; before the line, resulting in text which looks similar to:

<pre class="lang:ruby decode:true "># "-o ControlMaster=auto "+
# "-o ControlPath=#{controlpath} " +
# "-o ControlPersist=10m "+</pre>

Also, so that the cwRsync binary can find the local files on the C drive modify the path generation around line 43 to read:

<pre class="lang:ruby decode:true ">if Vagrant::Util::Platform.windows?
hostpath = "/cygdrive"+ Vagrant::Util::Platform.cygwin_path(hostpath) end</pre>

Save the file and you are now ready to configure Vagrant to create virtual machines within vSphere.
  
Vagrant will not be able to use the previous Linux box with vSphere but will instead need a template to be defined within the target vSphere environment. However a vagrant box will still need to be specified. So install a dummy box for use with vSphere using the following command:

<pre class="lang:batch decode:true ">C:\Users\home\vagrant-test&gt; vagrant box add atlas_shrugged/vsphere_dummy --name vsphere-dummy</pre>

Next edit the Vagrantfile to include configuration parameters for the vSphere environment:

<pre class="lang:default decode:true ">config.vm.provider :vsphere do |vsphere, override|
  override.vm.box = "vsphere-dummy"
  vsphere.host = "vSphere host IP"
  vsphere.insecure = true # don't verify the server SSL/TLS cert
  vsphere.compute_resource_name = "Example SILVER Cluster" 
  vsphere.resource_pool_name = "Example LAB 1"
  vsphere.customization_spec_name = "Vagrant_Linux-LAB 1-spec" 
  vsphere.template_name = "Templates/Vagrant/Ubuntu-14.04-x86_64-vagrant" 
  vsphere.vm_base_path = "Example CLOUD/SILVER_Cluster/VRM/ICT/LAB 1" 
  vsphere.vlan = "LAB-1-LAN-1 (1002)"
  vsphere.user = "domain\\your-user-name" # the '\\' double slash is required to 'escape' the slash when the value is passed to vsphere
  vsphere.password = "your-pass-word" 
end</pre>

Vagrant now has the information it needs to interface with vSphere on your behalf and create the virtual machine as it did with Virtualbox. To recreate the virtual machine in vSphere use the following vagrant command:

<pre class="lang:batch decode:true ">C:\Users\home\vagrant-test&gt; vagrant up --provider vsphere</pre>

This should connect to vSphere and deploy the virtual machine using the specified VMware template. Once completed Vagrant will copy all files in the vagrant-test directory to the remote server and allow you to connect to the remote server as you did earlier. When finished with the virtual machine you can equally execute &#8216;vagrant destroy&#8217; to delete the virtual machine.
  
Happy vming.

## Reference:

  * [Vagrant documentation](https://www.vagrantup.com/docs/)
  * [Vagrant tutorial](https://adamcod.es/2014/09/23/vagrant-ansible-quickstart-tutorial.html)
  * [Vagrant-VSphere documentation](https://github.com/nsidc/vagrant-vsphere)
