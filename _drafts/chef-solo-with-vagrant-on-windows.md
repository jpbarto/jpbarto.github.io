---
title: Chef Solo with Vagrant on Windows
---
Chef Solo with Vagrant on Windows
This article presumes you have read and followed the guide Vagrant on Windows Installation
To be able to provision machines created using Vagrant on Windows you will need to install a provisioning system such as Salt, Ansible, Puppet or Chef. Most of these do not actively support Windows however the Chef system is currently supported on Windows. What follows are the instructions for getting Chef installed onto a Windows device so that the 'Chef Solo' provisioner can be used from Vagrant.
To begin install the Chef Client onto Windows.
Execute the downloaded installer accepting the default options and afterward, reboot the Windows device.
After the device has rebooted the Chef installation should now be reflected in your command environments PATH.
You will see 2 new directories:
C:\chef
and
C:\opscode
Within C:\opscode there will be the Chef installation. All of the Chef executables can be found in C:\opscode\chef\bin. The C:\chef directory will be where recipes are installed for use by Vagrant.
To get started look on the Chef Supermarket for a recipe to be installed. This tutorial will use the 'apache2' recipe.
Before the recipe can be used by Vagrant it must first be installed. We will use the Chef utitlity 'knife' to download and install the cookbook. It will by default install recipes into
C:\chef\cookbooks which does not currently exist. The cookbooks directory will need to be a git repository so to prepare execute the following commands: C:> mkdir c:\chef\cookbooks
C:> cd c:\chef\cookbooks
C:> git init
C:> echo > README
C:> git add README
C:> git commit -m "init and commit of empty cookbooks repo"
After the git repository has been initialized use Knife to download and install the desired cookbooks: C:> knife cookbook site install apache2
After that concludes the apache2 cookbook should exist within c:\chef\cookbooks\apache2. To use it modify your Vagrantfile to include the following provisioning lines: config.vm.provision "chef_solo" do |chef|
chef.cokbooks_path = "c:\\chef\\cookbooks"
chef.add_recipe = "apache2" end
Now run vagrant up and the box will be created and the Chef recipe apache2 executed on the created virtual machine.
