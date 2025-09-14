+++
author = ["Kuzey Koç"]
date = 2025-09-10T00:00:00+03:00
tags = ["-F", "kubernetes"]
draft = false
+++

## How-to {#how-to}

-   [kubernetes secret file example]({{< relref "15d012dd-8153-4b67-9c4e-10134a629a7e" >}})
-   [temporarily expose argocd from kubernetes cluster]({{< relref "42cc5dca-4842-4699-827b-87fd83c1c7b9" >}})
-   [create an RBAC rule]({{< relref "47212944-c806-4640-927d-8666c640e950" >}})
-   [kubernetes configmap file example]({{< relref "4bc961c6-aaa2-4105-9cf0-29f1a99bb636" >}})
-   [kubernetes daemonset file example]({{< relref "604ba3af-3520-4af4-9ea1-2d34c982ec14" >}})
-   [set default namespace in kubernetes]({{< relref "66b97f48-a63f-4caa-90e2-95aab48be9f5" >}})
-   [check current namespace in kubernetes]({{< relref "6f82b6c9-ef95-422b-8f42-3346280397a6" >}})
-   [access web interface of a service that runs inside kubernetes]({{< relref "78577a00-4694-4dc7-b882-bf0ce7afaa66" >}})
-   [kubernetes deployment file example]({{< relref "8b83cb7b-9783-42bd-b3df-2a865ff7497d" >}})
-   [list all configmaps in kubernetes]({{< relref "90624a6b-b6d9-4471-ab53-19e5f9aaf452" >}})
-   [use nodeport instead of load balancer in kubernetes ingress]({{< relref "94cdb293-e7b8-4dbb-b2d0-f1f86c93d2c5" >}})
-   [list all secrets in kubernetes]({{< relref "b6e5204d-f95a-458a-b6cc-fa3e69b16aef" >}})
-   [attach to kubernetes pod's shell]({{< relref "be29e7e9-2985-494c-b65c-865489810a3c" >}})
-   [delete a kubernetes pod]({{< relref "c669bb8f-66ee-42cd-b0df-a15edc8d7836" >}})


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


### Videos {#videos}

-   [Complete Kubernetes Course - From BEGINNER to PRO - YouTube](https://www.youtube.com/watch?v=2T86xAtR6Fo)
-   [Kubernetes Crash Course for Absolute Beginners {NEW} - YouTube](https://www.youtube.com/watch?v=s_o8dwzRlu4)


### Books {#books}


### Articles {#articles}


## Page Renderer <span class="tag"><span class="noexport">noexport</span></span> {#page-renderer}

<a id="code-snippet--render-how-to"></a>
```emacs-lisp
(mapconcat
 (lambda (node)
   (format "- [[id:%s][%s]]"
           (org-roam-node-id node)
           (org-roam-node-title node)))
 (org-roam-ql-nodes
  '(tags "kubernetes" "_z"))
 "\n")
```

<a id="code-snippet--render-flashcards"></a>
```emacs-lisp
(mapconcat
 (lambda (node)
   (format "- [[id:%s][%s]]"
           (org-roam-node-id node)
           (org-roam-node-title node)))
 (org-roam-ql-nodes
  '(tags "kubernetes" "_f"))
 "\n")
```

[^fn:1]: [ChatGPT](https://chatgpt.com/)
[^fn:2]: [Kubernetes Volumes explained | Persistent Volume, Persistent Volume Claim &amp; S...](https://www.youtube.com/watch?v=0swOh5C3OVM)
