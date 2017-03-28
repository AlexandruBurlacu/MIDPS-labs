# VCS and working with CLIs

## Intro
For this laboratory work we will do the following:
- Connect to it via ssh to a remote server, namely to a Hadoop cluster provided by IBM
- Generate an ssh public key and add it to the GitHub project
- Work with git

The prerequisites are:
- The cluster, from [here](https://my.imdemocloud.com)
- Git
- some compilers and interpreters (gcc, g++, python, maybe something else) that are already there

## Act 1: Connect via SSH

Here everything is pretty simple: first we need an account on my.imdemocloud.com `>>=` get the credentials `>>=` type on our local machine the command `ssh <username>@systemt.datascientistworkbench.com` where _username_ is the name given by IBM for your account  `>>=` you are done, congrats!

![](./res/screen_1.png)

[NOTE] `>>=` is the __bind__ operator in Haskell language that is used with Monads.
To make it less geeky, it's equivalent to `.then` for JavaScript Promises.

## Act 2: The mighty Git

To initialize a directory you must run `git init`.
To make it track a remote repository `git remote add origin git@github.com:AlexandruBurlacu/MIDPS-labs.git`.
It is handy to to set the remote repo as the default destination for pushes from local, for this run `git push -u origin <remote_branch_name>`
To add a public RSA key there:
- generate a key with ssh-keygen ![](./res/screen_2.png)
- copy and add it into the remote repository ![](./res/screen_3.png)

To merge in the actual branch a single commit from another branch use `git cherry-pick <commit_from_another_branch SHA1>` ![](./res/screen_4.png)
To switch between branches run `git checkout <branch_name>`
To rollback a commit we have several solutions:
- `git reset HEAD` if you don't want the changes to be purged from the branch
- `gir reset --hard HEAD` if you DO want commited changes to be purged
- `git revert` rollbacks by 1 commit

Also there's `git rebase <branch_name>` which in some sense places one branch atop another, this way preserving commit history of both. ![](./res/screen_5.png)




## Act 3: Running the app remotely

For this laboratory work we will write, compile and run a ~~MapReduce job~~ some Hello World programs on a Hadoop cluster with 32 physical cores and 256 GB of RAM. ~~Sounds good, doesn't it?~~ I know, it sounds incredibly awful, I will explain during the next laboratory work how such thing happend.

So, we created some bash scripts that firs compile, then run the hello world programs written in C and Java, and then these scripts purge unwanted files like `.class` or executables.
