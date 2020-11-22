# Simplee.Fraktaal
A project for distributed algorithms.

[![Build status](https://ci.appveyor.com/api/projects/status/sn12cou91meiu35e/branch/main?svg=true)](https://ci.appveyor.com/project/veminovici/fraktaal/branch/main) 
[![Actions Status](https://github.com/veminovici/fraktaal/workflows/Build/badge.svg)](https://github.com/veminovici/csp-generator/actions)
[![F# Language](https://img.shields.io/github/languages/top/veminovici/fraktaal?color=%23b845fc)](https://img.shields.io/github/languages/top/veminovici/fraktaal?color=%23b845fc)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://img.shields.io/badge/License-Apache%202.0-blue.svg)
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

### 2.3. Breadth First
Implements the [breadth first](https://github.com/veminovici/fraktaal/blob/main/src/fraktaal/traversal/bf.fs) algorithm in which we traverse the graph of processes using the breadth first policy.

### 2.4. Depth First
Implements the [depth first](https://github.com/veminovici/fraktaal/blob/main/src/fraktaal/traversal/bf.fs) algorithm in which we traverse the graph of processes using the depth first policy.

### 2.5. Ring
Implements the [ring generation](https://github.com/veminovici/fraktaal/blob/main/src/fraktaal/traversal/ring.fs) algorithm in which we traverse the graph of processes and builds a ring where a token can be send from one process to another which process has a chance to consume the token.

<br />

## 3. Election Algorithms

### 3.1 Leader in Unidirectional Ring
Implements the [leader election](https://github.com/veminovici/fraktaal/blob/main/src/fraktaal/election/leaderUR.fs) algorithm in which we select a leader form a graph of processes.