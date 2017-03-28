# VCS and working with CLIs

## Intro
For this laboratory work we will do the following:
- Set up a VM, namely an Ubuntu 16.04 via VirtualBox tool.
- Connect to it via ssh
- Generate an ssh public key and add it to the GitHub project
- Work with git

The prerequisites are:
- Virtual Box. [here](https://www.virtualbox.org/wiki/Downloads)
- Ubuntu 16.04, x64 version. [here](https://www.ubuntu.com/download/desktop)
- Git
- some compilers and interpreters (gcc, g++, python, maybe something else)

## Act 1: VM

Here everything is pretty simple: first download VirtualBox `>>=` install it `>>=` create a new VM `>>=` set the virtual disk size to 12 GB `>>=` run it for the first time `>>=` add the downloaded `.iso` file (from the Ubuntu official size) when the wizard asks for an disk image `>>=` install Ubuntu `>>=` you are done, congrats!

In order to make the VM visible within the LAN, we need to setup a VirtualBox Host Only Network. For this we pick the __File -> Preferences__ menu, and there in the __Network__ submenu, __Host-Only Networks__, add a new network. The default IPv4 will be 182.168.56.1.

[NOTE] `>>=` is the __bind__ operator in Haskell language that is used with Monads.
To make it less geeky, it's equivalent to `.then` for JavaScript Promises.

## Act 2: ssh
Now that we can access the VM from the outside world, we run the following command:
`ssh <username>@<IPv4 of VM>`
![](./res/screen_1.png)
