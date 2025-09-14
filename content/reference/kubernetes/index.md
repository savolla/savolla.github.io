+++
title = "Kubernetes"
author = ["Kuzey Koç"]
date = 2025-09-10T00:00:00+03:00
tags = ["-F", "kubernetes"]
draft = false
+++

You can download the PDF version of this post


## Applied Notes {#applied-notes}

:results:


### kubernetes secret file example {#kubernetes-secret-file-example}

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: changeme
  type: Opeque
  data:
    username: c29tZSBzZWNyZXQgdmFsdWU=
    password: c29tZSBzZWNyZXQgcGFzc3dvcmQ=
```

-   [encode string value in base64]({{< relref "092a600a-b12e-4a8c-82e2-ddc418d4373c" >}}) and put encoded values in data section

---


### temporarily expose argocd from kubernetes cluster {#temporarily-expose-argocd-from-kubernetes-cluster}

```shell
kubectl get svc -n argocd # get argocd service (with port 80 or 443)
kubectl port-forward svc/argocd-server -n argocd 8080:443 # start port forwarding
```

-   access the service <https://localhost:8080>
-   login as admin
-   [get admin password of argocd in openshift]({{< relref "7eb6d892-979d-4b13-9631-c5cc1bed6f58" >}})

---


### create an RBAC rule {#create-an-rbac-rule}

-   make `alice` user only **list** pods under `dev` namespace
-   define role
    ```yaml
      apiVersion: rbac.authorization.k8s.io/v1
      kind: Role
      metadata:
        namespace: dev
        name: pod-reader
        rules:
    ​      - apiGroups: [""]
            resources: ["pods"]
            verbs: ["get", "list"]
    ```
-   bind this role to `alice`
    ```yaml
      apiVersion: rbac.authorization.k8s.io/v1
      kind: RoleBinding
      metadata:
        name: read-pods-binding
        namespace: dev
        subjects:
    ​      - kind: User
            name: alice
            apiGroup: rbac.authorization.k8s.io
            roleRef:
              kind: Role
              name: pod-reader
              apiGroup: rbac.authorization.k8s.io
    ```

---


### kubernetes configmap file example {#kubernetes-configmap-file-example}

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: changeme
  data:
    variable0: "some value"
    variable1: "some value"
```

---


### kubernetes daemonset file example {#kubernetes-daemonset-file-example}

-   the following manifest will create a pod called `my-image:latest` <span class="underline">in every node</span>

<!--listend-->

```yaml
apiVersion: apps/v1
kind: DaemonSet
metadata:
  name: my-daemonset
  spec:
    selector:
      matchLabels:
        app: my-daemon
        template:
          metadata:
            labels:
              app: my-daemon
              spec:
                containers:
                  - name: my-daemon-container
                    image: my-image:latest
```

---


### set default namespace in kubernetes {#set-default-namespace-in-kubernetes}

the following example sets "cassandra" as a default namespace for `admin` user

```shell
kubectl config set-context --current --namespace=cassandra
```

---


### check current namespace in kubernetes {#check-current-namespace-in-kubernetes}

```shell
kubectl config get-contexts
```

| CURRENT | NAME                        | CLUSTER               | AUTHINFO                    | NAMESPACE |
|---------|-----------------------------|-----------------------|-----------------------------|-----------|
| \*      | admin@talos-proxmox-cluster | talos-proxmox-cluster | admin@talos-proxmox-cluster | cassandra |

---


### access web interface of a service that runs inside kubernetes {#access-web-interface-of-a-service-that-runs-inside-kubernetes}

-   by **port forwarding**
    ```shell
    oc get services
    # check your service's PORT number (say it's 80)
    # say your service name is jenkins
    oc port-forward svc/jenkins 3000:80
    ```
    notice that port `3000` is the port on your host machine and port `80` is that jenkins exposes

---


### kubernetes deployment file example {#kubernetes-deployment-file-example}

-   this example is for nginx deployment

<!--listend-->

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: nginx-deployment
  labels:
    app: nginx
    spec:
      replicas: 3
      selector:
        matchLabels:
          app: nginx
          template:
            metadata:
              labels:
                app: nginx
                spec:
                  containers:
                    - name: nginx
                      image: nginx:1.14.2
                      ports:
                        - containerPort: 80
```

---


### list all configmaps in kubernetes {#list-all-configmaps-in-kubernetes}

```shell
kubectl get configmap
```

---


### use nodeport instead of load balancer in kubernetes ingress {#use-nodeport-instead-of-load-balancer-in-kubernetes-ingress}

-   patch the ingress

<!--listend-->

```shell
kubectl patch svc ingress-nginx-controller -n ingress-nginx \
    -p '{"spec": {"type": "NodePort"}}'
```

---


### list all secrets in kubernetes {#list-all-secrets-in-kubernetes}

```shell
kubectl get secret
```

---


### attach to kubernetes pod's shell {#attach-to-kubernetes-pod-s-shell}

-   `kubectl get pod` and get the name of your taget pod

<!--listend-->

```shell
kubectl exec -it $YOUR_POD_NAME -- /bin/sh
```

---


### delete a kubernetes pod {#delete-a-kubernetes-pod}

```shell
kubectl delete -f /path/to/file.yaml
```

:end:


## Notes {#notes}

-   kubernetesin mimarisinde 2 temel kavram vardır. **control plane** ve **worker node**
    -   control plane
        -   kubernetes spesifik proseslerin çalıştığı master.
        -   control plane içinde çalışan önemli prosesler
            -   api server
                -   kısa ismi **api**
            -   controller manager
                -   kısa ismi **c-m**
                -   cluster içini sürekli izler ve bir sorun çıktığında onarır
                -   bir pod ya da container öldü diyelim. hemen onu tamir eder. replicaset buna örnek verilebilir
            -   scheduler
                -   kısa ismi **sched**
                -   uygulamaları, worker nodelara dağıtır. bunu yaparken de workload ya da cpu ve ram kullanımlarına göre yapar
            -   etcd
                -   key value store. yani kubernetes clusterının ayarlarını saklar. configürasyonu, her nodun statelerini vs tutar
                -   ayrıca bir clusterın backupını almak istiyorsak etcd'den almamız gerekir
        -   cluster içinde en az 2 control plane olması gerekir. çünkü biri düşerse diğer ayakta kalacak şekilde kurulması gerekir
    -   worker node
        -   kullanıcı uygulamalarını çalıştığı ve control plane tarafından yönetilen serverlar
-   kubernetes için var olan web tabanlı arayüzler vardır. bunlardan biri **kubernetes dashboard**
-   kubernetes komponentleri
    -   pod
        -   kubernetesdeki en temel birimdir
        -   bir ya da birden çok containerı barındırır
    -   service
        -   pod içinde çalışan konteynerların kendi ip adresleri vardır ancak ephemeral oldukları için bu konteynerlar replicaset tarafından değiştirilir. bu olunca da ip adresi değişir. ip adresi değşse bile diğer uygulamaların bu pod içinde çalışan uygulamaya erişiminin devam edebilmesi için **service** diye bir k8s komponenti geliştirildi.
        -   service, bir pod'a atanmış statik ip adresi gibi düşünülebilir
        -   servis aynı zamanda bir **load balancer** gibi de davranır.
    -   ingress
        -   pod içinde çalışan bir service browserdan erişmek istediğimizde ingres kullanabiliriz. burada ingress, service ile iletişim kurar. service ise gerekli konteynerın servisini ingresse açar.
    -   configmap
        -   pod içinde çalışan uygulamanın ayarları ya da ortam değişkenlerini buraya yazabiliriz ve k8s buradan okuyup işlem yapabilir
        -   configmap kullanmanın avantajı, bu değişkenleri dockerfile'a yazmayıp, uygulamayı her seferinde build etmememizi sağlar.
    -   secret
        -   configmap gibi ancak daha hassas verileri saklamak için kullanılır. örneğin api tokenları, şifreler gibi.
        -   secretlar **plain text** olarak etcd'de saklanır. api erişimi olan herkes bu bilgilere erişip secretları okuyup değiştirebilir. bu yüzden secretları encrypt etmek için 3rd party araçlar kullanılması gerekir. Vault gibi
    -   volume
        -   pod içinde çalışan ve veri üreten konteynerların bu verileri **persistence** yapması için bir disk alanı gerekir. bunun için volumelar vardır. veritabanı gibi servislerin kesinlikle buna ihtiyacı vardır.
        -   volume aynı makinede de saklanabilir, cloud providerların bir storage servisinde de saklanabilir
    -   deployment
        -   pod, konteynerlar arası bir soyutlama yapıyorsa, deployment da podlar üstünden bir soyutlama yapan kubernetes konseptidir.
        -   statei ya da bir volume'e ihtiyaç duymayan uygulamalar için kullanılır
        -   genellikle direk podlar ile değil de deploymentlar ile çalışırız
    -   stetefulset
        -   **sts** kısa ismi
        -   database uygulamaları için özel olarak geliştirilmiştir. **mysql**, **mongodb**, **elasticsearch**. bu gibi uygulamalar <span class="underline">her zaman stateful sets ile</span> deploy edilmelidir. deployment ile değil
        -   database applikasyonlarını stateful set ile deploy etmek biraz uğraştırıcıdır. bu yüzden veritabanı gibi uyglamaları kubernetes clusterı dışında tutmak daha mantıklıdır. yani veritabanını yine deployment ile kurup asıl datbase bağlantısını dışardaki veritabanına göndermek en mantıklısıdır.
-   kubernetes has 3 different pluggable components
    -   CRI (container runtime interface)
        -   containerd and cri-o
        -   docker was removed
    -   CNI (container networking interface)
        -   k8s has k-proxy by default for cluster networking but there are different puggable services as well
        -   flannel
        -   calico
        -   cilium
    -   CSI (container storage intreface)
        -   this is for storage in cluster
-   **DaemonSet**
    -   **DaemonSet** her node'da çalışması gereken servislerin tanımlandığı bir deployment türüdür. Her bir node'da <span class="underline">sistem düzeyinde</span> çalışması gereken servisler için kullanılır. Örnek olarak; **log toplama** (Fluentd, Logstash), **node monitoring** (prometheus node exporter), **container network interface** (calico, flanel) ve **security agents** gibi "node" ile ilgili olan servisler için kullanılır.&nbsp;[^fn:1]

    -   Yeni bir node cluster’a eklenirse, DaemonSet otomatik olarak o node’da da bir Pod oluşturur. Node cluster’dan silinirse, ilgili Pod da silinir. İstersen sadece belirli label’lara sahip node’lara Pod yerleştirebilirsin (nodeSelector, affinity vb.).&nbsp;[^fn:1]

    -   Bir daemonset oluşturmak için de şu örneğe bakabilirsin [kubernetes daemonset file example]({{< relref "604ba3af-3520-4af4-9ea1-2d34c982ec14" >}})
    -   **DaemonSet ve Deployment Farkları**&nbsp;[^fn:1]

        | özellik         | DaemonSet         | Deployment                  |
        |-----------------|-------------------|-----------------------------|
        | pod sayısı      | her node'a 1 tane | belirli sayıda (replicaset) |
        | node'a bağlılık | evet              | hayır                       |
        | usecase         | sistem/altyapı    | aplikasyon                  |
-   **Kubernetes Volumes**
    -   kubernetes does not give you data persistency out of the box&nbsp;[^fn:2]
    -   **Persistent Volume**
        -   belongs to cluster itself. it is a dedicated disk
    -   **Persistent Volume Claim**
    -   **Storage Class**


## Flashcards {#flashcards}


## Resources {#resources}


### Docs {#docs}

-   [Kubernetes Documentation | Kubernetes](https://kubernetes.io/docs/home/)


### Videos {#videos}

-   [Complete Kubernetes Course - From BEGINNER to PRO - YouTube](https://www.youtube.com/watch?v=2T86xAtR6Fo)
-   [Kubernetes Crash Course for Absolute Beginners {NEW} - YouTube](https://www.youtube.com/watch?v=s_o8dwzRlu4)


### Books {#books}


### Articles {#articles}

[^fn:1]: [ChatGPT](https://chatgpt.com/)
[^fn:2]: [Kubernetes Volumes explained | Persistent Volume, Persistent Volume Claim &amp; S...](https://www.youtube.com/watch?v=0swOh5C3OVM)
