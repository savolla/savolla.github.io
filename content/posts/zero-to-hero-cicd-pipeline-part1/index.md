+++
title = "Zero to Hero CICD Pipeline | Part 1"
author = ["Kuzey Koç"]
date = 2025-09-02T00:00:00+03:00
tags = ["devops", "-F"]
draft = false
+++

## Introduction {#introduction}

you can find the pipeline [here](https://www.canva.com/design/DAGRUMFFs3c/lNFwVoe4ymvXjbvVBlM0LQ/view?utm_content=DAGRUMFFs3c&utm_campaign=designshare&utm_medium=link&utm_source=editor)

Hello guys! In this video, we'll be building a complete CI/CD pipeline from the ground up. I'll guide you through setting up and using a variety of powerful tools including Terraform, Proxmox, NixOS, Jenkins, SonarQube, Docker, Kubernetes, and many more.

[Display additional tools on screen.]

We'll start by creating this pipeline from scratch, beginning with Infrastructure as Code. Using Terraform and Proxmox, we’ll spin up our servers, then proceed to install essential tools such as Jenkins, Kubernetes, ArgoCD, SonarQube, Nexus, and monitoring solutions like Prometheus and Grafana.

Finally, we'll deploy a sample application using the full pipeline.

This will be a multi-part tutorial series, so stay tuned for more!


## Pipeline Showcase {#pipeline-showcase}

This is the pipeline we'll be building throughout this tutorial series. I've intentionally made it complex to maximize the learning experience. Throughout the setup, I’ve gained a lot of valuable insights, and now, I’m excited to share that knowledge with you. My goal is to help you deepen your understanding and take your skills to the next level.

[start explaining pipeline from start to finish]


## A Note For My Environment {#a-note-for-my-environment}

Let me quickly explain my environment so you’re comfortable following along.

For this tutorial, I’ll be using a special Linux distribution called NixOS. The reason I chose NixOS is because of its powerful package manager and overall stability. I'll walk you through how to install packages and use some of its key features. However, if you prefer using another Linux distro like Arch or Debian, that’s perfectly fine. It won’t make a big difference. But If you're on Windows or macOS, I won’t be covering the installation of tools on those systems, so you'll need to handle that part on your own.

For my text editor, I'll be using Emacs, but feel free to use VSCode or any other tool you’re comfortable with.

To manage my terminal sessions more efficiently, I’ll be using **tmux**. It's an essential tool for anyone who spends a lot of time in the terminal, as it allows you to manage multiple sessions within a single terminal window. Plus, it preserves your SSH connections even if you accidentally close the terminal. For this tutorial, I’ll be using two **tmux** sessions: one for editing text and typing commands in Emacs, and another for managing SSH connections. Don’t worry, I’ll explain everything as we go to make it all crystal clear.

To help you follow along more easily, I’ll also be running a tool called **screenkey**, which will display my keystrokes in the bottom right corner.


## Code and Documentation {#code-and-documentation}

I want to make things as easy as possible for you. All the code and documentation from this series will be available in one place. Just head over to **savolla.github.io/cortex**, where you’ll find my personal knowledge base. Click on the ‘Videos’ section, find the video you’re watching, and you’ll have access to all the code and resources.

In the next episode, we’ll begin by installing Proxmox as a virtual machine and use Terraform to spin up our servers
