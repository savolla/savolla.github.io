+++
title = "Kubernetes"
author = ["Kuzey Koç"]
date = 2025-09-10T00:00:00+03:00
tags = ["-F", "kubernetes"]
draft = false
+++

You can get the PDF version of this article [here](/ox-hugo/index.pdf) 📎


## <span class="section-num">1</span> Applied Notes {#applied-notes}


## <span class="section-num">2</span> Notes {#notes}

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


## <span class="section-num">3</span> Flashcards {#flashcards}


## <span class="section-num">4</span> Resources {#resources}


### <span class="section-num">4.1</span> Docs {#docs}

-   [Kubernetes Documentation | Kubernetes](https://kubernetes.io/docs/home/)


### <span class="section-num">4.2</span> Videos {#videos}

-   [Complete Kubernetes Course - From BEGINNER to PRO - YouTube](https://www.youtube.com/watch?v=2T86xAtR6Fo)
-   [Kubernetes Crash Course for Absolute Beginners {NEW} - YouTube](https://www.youtube.com/watch?v=s_o8dwzRlu4)


### <span class="section-num">4.3</span> Books {#books}


### <span class="section-num">4.4</span> Articles {#articles}

[^fn:1]: [ChatGPT](https://chatgpt.com/)
[^fn:2]: [Kubernetes Volumes explained | Persistent Volume, Persistent Volume Claim &amp; S...](https://www.youtube.com/watch?v=0swOh5C3OVM)
