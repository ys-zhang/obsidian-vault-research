---
title: Directed greybox fuzzing
authors: Marcel Böhme, Van-Thuan Pham, Manh-Dung Nguyen, Abhik Roychoudhury
year: 2017
---


#program-test #fuzzing #academic-paper

# Problem

Existing [[graybox fuzzing]] fuzzers cannot be effectively directed.

>In this paper, we introduce** Directed Greybox Fuzzing (DGF)** which generates inputs with the objective of reaching a given set of target program locations efficiently. We develop and evaluate a [[Simulated annealing]]-based power schedule that gradually assigns more energy to seeds that are closer to the target locations while reducing energy for seeds that are further away.

Given a target line or block of code, the problem can be rephrased as **find out a seed s.t. its path passes the target code, i.e. a reachability problem.**

This is useful when only the changed code blocks need to be tested, including
- [[patch testing]]

# Idea

> On a high level, we cast reachability as an optimization problem and employ a specific meta-heuristic to minimize the distance of the generated seeds to the targets.

DGF casts the reachability of target locations as optimization problem while existing directed (whitebox) fuzzing approaches cast reachability as iterative constraint satisfaction problem.

Directed greybox fuzzing is effectively directed and efficiently complements symbolic execution-based directed fuzzing.


![[greybox-fuzzing-arch.excalidraw]]

In the **exploitation phase**, **AFLGo** generates substantially more new inputs from seeds that are closer to the target—essentially not wasting precious time fuzzing seeds that are too far away.

**AFLGo** slowly transitions from the** exploration phase** to the **exploitation phase**, according to the annealing function implemented as **power schedule**


# Measure Energy

>We define an inter-procedural measure of distance (i.e., seed to target locations) that is fully established at instrumentation-time and can be efficiently computed at runtime.

The distance computation requires finding the shortest path to the target nodes in the call graph and the intra-procedural control-flow graphs which are readily available in [[LLVM]].

## Notations
| Notation | Concept         |
| -------- | --------------- |
| $T_f$    | target function |
| $T_b$    | target [[basic block]]                |
