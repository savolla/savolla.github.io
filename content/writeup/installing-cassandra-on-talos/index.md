+++
title = "Installing Cassandra on Talos"
author = ["Kuzey Koç"]
date = 2025-09-11T00:00:00+03:00
tags = ["helm", "k8ssandra", "cassandra", "kubernetes", "-writeup", "talos"]
draft = false
+++

-   intoduction
    -   about my environment
        -   check this article for installing talos os on proxmox
        -   proxmox
        -   talos
        -   k8ssandra
            -   k8ssandra is a kubernetes operator for cassandra
        -   nixos
            -   throughout the write-up I'm going to use nixos as my local machine and also as a controller. I choose this distrob because it provides features like **flakes** and **devshells**. see [Flakes - NixOS Wiki](https://nixos.wiki/wiki/flakes) for more information
            -   &lt;share your direnv and flake.nix&gt;
    -   why I do it this way?
        -   pros and cons of installing cassandra on kubernetes
            -   &lt;a table&gt;&nbsp;[^fn:1]
        -   a little bit about k8ssandra
-   login to nixos
-   start your talos nodes
    -   `ssh` into your proxmox server
        ```shell
        ssh root@192.168.1.250
        ```
    -   starting all talos nodes in my proxmox server
        I label my talos nodes like the following
        ```shell
        300 talos-master-01      running    2048              10.00 106311
        400 talos-worker-01      running    2048              32.00 106453
        401 talos-worker-02      running    2048              32.00 106541
        402 talos-worker-03      running    2048              32.00 106619
        1400 template-talos-worker stopped    2048              32.00 0
        ```
        I also have a template for talos-worker nodes. I'll exclude it using `grep -v` in the following command. I "bulk start" every talos node with the following loop
        ```shell
        for node in $(qm list | grep talos | grep -v template | awk '{print $1}');
        do
            qm start $node
        done
        ```
        Since `qm` doesn't support labels and only use ids for vms I also needed to `awk` the output

    -   let's do a quick sanity check on our talos cluster just to be sure everything works as expected
        ```shell
        kubectl get nodes -o wide
        ```

        ```shell
        NAME            STATUS   ROLES           AGE   VERSION   INTERNAL-IP     EXTERNAL-IP   OS-IMAGE          KERNEL-VERSION   CONTAINER-RUNTIME
        talos-5sv-2k5   Ready    <none>          11d   v1.33.0   192.168.1.111   <none>        Talos (v1.10.1)   6.12.25-talos    containerd://2.0.5
        talos-d1h-j43   Ready    <none>          11d   v1.33.0   192.168.1.109   <none>        Talos (v1.10.1)   6.12.25-talos    containerd://2.0.5
        talos-fsq-dfl   Ready    <none>          11d   v1.33.0   192.168.1.106   <none>        Talos (v1.10.1)   6.12.25-talos    containerd://2.0.5
        talos-t8d-gb7   Ready    control-plane   11d   v1.33.0   192.168.1.107   <none>        Talos (v1.10.1)   6.12.25-talos    containerd://2.0.5
        ```
-   install traefik
    -   Before we install `k8ssandra` we need a reverse proxy just to expose our services and making is easier to route between them.
    -   I like to use `traefik` instead of `nginx` for exposing my webuis. it provides bunch of features such as a webui, auto https, automatic service discovery etc. let's add it to our helm repo
        ```shell
        helm repo add traefik https://helm.traefik.io/traefik
        helm repo update
        ```

    -   We'll need to tell **traefik** which services to expose. We'll use the following `yaml` file for that purpose&nbsp;[^fn:2] <sup>, </sup>[^fn:3]

    -   We're using Talos as our cluster OS, which enforces the **Pod Security Standards (PSS)**. This means we must assign different ports to each container, even if they have different names. So I assign different port names for each service.

    -   I also sort my containers by their `port` number for easier debugging.

    -   Now create a file named `traefik.yaml` and add the following into it
        ```yaml
        providers:
          kubernetesCRD:
            namespaces:
        ​      - default
          kubernetesIngress:
            namespaces:
        ​      - default

        ports:
          web:
            port: 80
            protocol: TCP
            expose:
              default: true
            exposedPort: 32080
            nodePort: 32080

          websecure:
            port: 443
            tls:
              enabled: true
            protocol: TCP
            expose:
              default: true
            exposedPort: 32443
            nodePort: 32443

          traefik:
            port: 8080
            protocol: TCP
            expose:
              default: true
            exposedPort: 32090
            nodePort: 32090

          sg-auth:
            port: 8081
            protocol: TCP
            expose:
              default: true
            exposedPort: 30081
            nodePort: 30081

          sg-rest:
            port: 8082
            protocol: TCP
            expose:
              default: true
            exposedPort: 30082
            nodePort: 30082

          sg-graphql:
            port: 8083
            protocol: TCP
            expose:
              default: true
            exposedPort: 30080
            nodePort: 30080

          cassandra:
            port: 9042
            protocol: TCP
            expose:
              default: true
            exposedPort: 32091
            nodePort: 32091

          cassandrasecure:
            port: 9142
            protocol: TCP
            expose:
              default: true
            exposedPort: 32092
            nodePort: 32092

        service:
          type: NodePort
        ```

    -   Talos Kubernetes (and other secure distros) enforce Pod Security Admission rules. In "restricted" mode, every container must explicitly declare its `securityContext.seccompProfile`. This is a security best practice and often not set by default in older Helm charts.&nbsp;[^fn:1]

    -   That's why we also need to add a `seccomp` profile to our config
        ```yaml
        deployment:
          podSecurityContext:
            seccompProfile:
              type: RuntimeDefault

          securityContext:
            seccompProfile:
              type: RuntimeDefault
        ```

    -   Now install traefik using this configuration.
        ```shell
        helm install traefik traefik/traefik -f traefik.yaml --namespace=default
        ```
    -   By the way I'm using `NodePort` for demonstration purposes. It should be `LoadBalancer`.

-   install k8ssandra
    -   K8ssandra is an open-source project that provides a full-stack, production-ready Apache Cassandra® deployment on Kubernetes. It bundles Cassandra with other tools for management, monitoring, and backup into a Kubernetes-native deployment model.&nbsp;[^fn:1]

    -   add the k8ssandra helm chart repo&nbsp;[^fn:4]
        ```shell
        helm repo add k8ssandra https://helm.k8ssandra.io/stable
        helm repo update
        ```

    -   create a file called `k8ssandra.yaml` and add the following into it
        ```yaml
        # k8ssandra.yaml
        cassandra:
          version: "4.1.3"
          datacenters:
        ​    - name: dc1
              size: 3
              storageConfig:
                cassandraDataVolumeClaimSpec:
                  accessModes: ["ReadWriteOnce"]
                  storageClassName: "default"  # Make sure this exists in Talos
                  resources:
                    requests:
                      storage: 5Gi
              racks:
        ​        - name: rack1

        stargate:
          enabled: true
          replicas: 1
          heapMB: 512
          cpuReqMillicores: 500
          cpuLimMillicores: 1000
          service:
            type: NodePort
            ports:
              graphql:
                nodePort: 30080
              auth:
                nodePort: 30081
              rest:
                nodePort: 30082

        medusa:
          enabled: false

        reaper:
          enabled: false

        prometheus:
          enabled: false

        grafana:
          enabled: false

        # SecurityContext (Talos PSS compliance)
        securityContext:
          seccompProfile:
            type: RuntimeDefault

        podSecurityContext:
          seccompProfile:
            type: RuntimeDefault
        ```

[^fn:1]: [ChatGPT](https://chatgpt.com/)
[^fn:2]: [K8ssandra First Touch - YouTube](https://www.youtube.com/watch?v=qlZVLEWzJq0&list=PL2g2h-wyI4Sq_6MQEVn4fFmjSJ10IwVrU&t=177s)
[^fn:3]: [traefik/traefik-helm-chart v27.0.0 on GitHub](https://newreleases.io/project/github/traefik/traefik-helm-chart/release/v27.0.0)
[^fn:4]: [Install prerequisites | K8ssandra, Apache Cassandra on Kubernetes](https://docs.k8ssandra.io/install/local/)
