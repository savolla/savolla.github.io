+++
title = "Dotfiles Management with Bare Git Repo"
author = ["Kuzey Koç"]
date = 2025-09-12T00:00:00+03:00
tags = ["git", "-F"]
draft = false
+++

## Create A Bare Git Repo {#create-a-bare-git-repo}

We'll setup a bare git repository under our `$HOME` directory called `.dotfile` first.

```shell
git init --bare $HOME/.dotfiles
```

Add the following into your `.zshrc` or `.bashrc`. We'll use `dotfile` alias for adding our configuration files to the bare repo. If you don't want this, then you can type the whole `git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME` command for each operation.

```shell
alias dotfiles='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
```

After adding this alias simply restart your terminal and continue

Normally you'll get bunch of untracked files when you do `dotfiles status`. We want to suppress that behavior. Just configure the repo to ignore untracked files

```shell
dotfiles config --local status.showUntrackedFiles no
```

Now you can start adding your dotfiles just like this;

```shell
dotfiles add .bashrc .zshrc
```

After you finish adding dotfiles you can commit changes by doing;

```shell
dotfiles commit -m "feat: added dotfiles"
```


## Create a New GitHub Repository {#create-a-new-github-repository}

This is completely optional and you may want to host your dotfiles on another platform like _GitLab_, _CodeBerg_ etc. But for this tutorial we'll use GitHub as an example

Create a new repository on github named "dotfiles"

Go back to your terminal and prepare your local repository for pushing.

```shell
dotfiles remote add origin git@github.com:savolla/dotfiles.git
dotfiles branch -M main
```

Now push your local bare git repo to github

```shell
dotfiles push -u origin main
```


## Install Dotfiles on a New System {#install-dotfiles-on-a-new-system}

You completed the "tracking" part of the process but you may want to get back and install these dotfiles on a freshly installed system later right?

Clone your repo to a new system using bare repo method

```shell
git clone --bare git@github.com:savolla/dotfiles.git $HOME/.dotfiles
```

Checkout to your home directory

```shell
git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME checkout
```

Please note that if you already have pre existing dotfiles such as `.bashrc` on a new system, git will refuse replacing them with your dotfiles. If you're okay with replacing those then just use `-f` after `checkout` parameter to force it

Ignore untracked files again

```shell
git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME config --local status.showUntrackedFiles no
```

Add the alias again if you didn't already

```shell
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
```

Now you should have your dotfiles setup on your new system!
