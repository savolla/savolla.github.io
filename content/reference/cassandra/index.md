+++
title = "Cassandra"
author = ["Kuzey Koç"]
date = 2025-09-08T00:00:00+03:00
tags = ["cassandra", "-F"]
draft = false
+++

## Notes {#notes}

-   cassandra is an **open source**, **nosql**, **distributed** database
-   developed by **apache**
-   cassandra is highly available and scalable database technology that is used by big tech companies like **Netflix**, **Hulu**, **Comcast** etc.
-   in **RDBMS** we model data first and then adapt our application to the data. In **cassandra** we first model our queries and then model data.
-   relational databases use ACID
-   ACID stands for **atomicity**, **consistency**, **isolation** and **durability**
    -   **atomicity**: all the statements in a transaction must success. if one of those transactions doesn't succeed then others don't as well.
    -   **consistency**:
    -   **isolation**: multiple transactions can be processed **at the same time** without delay
    -   **durability**: guarantees persistency of completed transactions even after a failure.
-   cassandra <span class="underline">does not</span> support **ACID**
-   cassandra is an **AP** system. meaning that it <span class="underline">sacrifices consistency</span> for **availability** and **partition-tolerance**
-   cassandra supports running inside a cluster. meaning that it can work in separate servers that connected via a network. this makes cassandra **partition-tolerant**. Therefore cassandra does not have **single point of failure** this way
-   **snitch**
    -   snitch is a cassandra term that makes cassandra nodes know about each other inside the same cluster or across multiple data centers.
    -   cassandra nodes don't have a master. so the only way to tell cassandra about the network topology is to use **snitch**
    -   there are multiple kinds of snitches in cassandra
        -   **SimpleSnitch**
            -   when you first install cassandra the **SimpleSnitch** will be enabled by default
            -   **SimpleSnitch** has a downside that it makes cassandra work only in the current datacenter. cassandra spreads across multiple data centers to increase high availability. so this is a downside
        -   **PropertyFileSnitch**
            -   use this if you have less than 40 cassandra nodes. because you'll have to setup ip addresses for each node. this is kinda tedious.
        -   **GossipPropertyFileSnitch**
-   **gossip**
    -   gossip is the name of **internal node-to-node** communication. nodes talk to each other to make their network alive. this is called "gossip"
    -   while gossip is internal communication, if we want to make cassandra do something like create tables or add data etc, this is called **external** communication.
-   cassandra uses **Murmur3** algorithm for **partitioning** tables across multiple nodes
-   **replication factor**
    -   this is similar to **replicaset** in kubernetes. you define how much replica of the node should exist. use high values like 5 for banking data


## Zettels {#zettels}

```emacs-lisp
(mapconcat
 (lambda (node)
   (format "- [[id:%s][%s]]"
           (org-roam-node-id node)
           (org-roam-node-title node)))
 (org-roam-ql-nodes
  '(tags "cassandra" "_z"))
 "\n")
```

-   [install cassandra on ubuntu server]({{< relref "8fc36f7c-c3db-4cdf-a0cf-7081180b5514" >}})


## Flashcards {#flashcards}

```emacs-lisp
(mapconcat
 (lambda (node)
   (format "- [[id:%s][%s]]"
           (org-roam-node-id node)
           (org-roam-node-title node)))
 (org-roam-ql-nodes
  '(tags "cassandra" "_f"))
 "\n")
```


## References {#references}

1.  [Apache Cassandra Documentation](https://cassandra.apache.org/doc/latest/)
2.  [Apache Cassandra Database – Full Course for Beginners - YouTube](https://www.youtube.com/watch?v=J-cSy5MeMOA)
3.  [Cassandra Tutorial - Complete NoSQL Database Course - YouTube](https://www.youtube.com/watch?v=8g-f9uPzW04)
4.  [How to Install Cassandra on Ubuntu (Linux) - YouTube](https://www.youtube.com/watch?v=-9P4CxRWL8c)
