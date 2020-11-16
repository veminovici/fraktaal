# Simplee.Fraktaal
A project for distributed algorithms.

<br />

## 1. Kernel
This is the implementation of the [kernel](https://github.com/veminovici/fraktaal/blob/main/src/fraktaal/core/kernel.fs) for the distributed systems. It defines all the interfaces and the kernel can spawn different processes. The implementation has a logger process which collects all the logs in the systems.

<br />

## 2. Traversal Algorithms
A collection of algorithms for different traversal scenarios. Each algorithm is implemented by a specialized actor.


### 2.1. Learner
Implements the [learning](https://github.com/veminovici/fraktaal/blob/main/src/fraktaal/traversal/learner.fs) algorithm in which all actors learn about the other nodes in the system and their links.

### 2.2. Spanning Tree
Implements the [spanning tree](https://github.com/veminovici/fraktaal/blob/main/src/fraktaal/traversal/sptree.fs) algorithm in which we build a spanning tree with the root in the starting process and the children being the rest of the processes.