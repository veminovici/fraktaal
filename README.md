# Simplee.Fraktaal
A project for distributed algorithms.

[![Build status](https://ci.appveyor.com/api/projects/status/sn12cou91meiu35e/branch/main?svg=true)](https://ci.appveyor.com/project/veminovici/fraktaal) 
[![Actions Status](https://github.com/veminovici/fraktaal/workflows/Build/badge.svg)](https://github.com/veminovici/fraktaal/actions)
[![Release](https://img.shields.io/github/v/release/veminovici/fraktaal?include_prereleases)](https://github.com/veminovici/fraktaal/releases/tag/V0.1-alpha)
[![F# Language](https://img.shields.io/github/languages/top/veminovici/fraktaal?color=%23b845fc)](https://github.com/veminovici/fraktaal)
[![GitHub repo size](https://img.shields.io/github/repo-size/veminovici/fraktaal)](https://github.com/veminovici/fraktaal)
[![License](https://img.shields.io/github/license/veminovici/fraktaal)](https://opensource.org/licenses/Apache-2.0)
[![Sponsors](https://img.shields.io/static/v1?label=Sponsor&message=%E2%9D%A4&logo=GitHub&color=red)](https://github.com/sponsors/veminovici)
<br />

[![Appveyor History](https://buildstats.info/appveyor/chart/veminovici/fraktaal?branch=main)](https://ci.appveyor.com/project/veminovici/fraktaal)
[![Github Actions](https://buildstats.info/github/chart/veminovici/fraktaal)](https://github.com/sponsors/veminovici)

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