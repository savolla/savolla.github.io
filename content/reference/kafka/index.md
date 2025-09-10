+++
title = "Kafka"
author = ["Kuzey Koç"]
date = 2025-09-10T00:00:00+03:00
tags = ["-F", "kafka"]
draft = false
+++

## Notes {#notes}

-   kafka can be used as a **message queue** or a **stream processing** system&nbsp;[^fn:1]
-   **message/record**
    -   kafka, "message" ve "record" terimlerini aynı anlamda kullanır
    -   mesajlar 4 ana bileşenden oluşur; **Headers**, **Key**, **Value** ve **Timestamp**


## Zettels {#zettels}

```emacs-lisp
(mapconcat
 (lambda (node)
   (format "- [[id:%s][%s]]"
           (org-roam-node-id node)
           (org-roam-node-title node)))
 (org-roam-ql-nodes
  '(tags "kafka" "_z"))
 "\n")
```


## Flashcards {#flashcards}

```emacs-lisp
(mapconcat
 (lambda (node)
   (format "- [[id:%s][%s]]"
           (org-roam-node-id node)
           (org-roam-node-title node)))
 (org-roam-ql-nodes
  '(tags "kafka" "_f"))
 "\n")
```


## References {#references}

[^fn:1]: [Kafka Deep Dive w/ a Ex-Meta Staff Engineer - YouTube](https://www.youtube.com/watch?v=DU8o-OTeoCc)
