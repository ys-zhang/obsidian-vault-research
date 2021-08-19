# Assess a schedule algorithm

![[Pasted image 20210812204600.png]]

### Turnaround Time (about performance)
周转时间（turnaround time）。任务的周转时间定义为任务完成时间减去任务到达系统的时间.

> 大概等价于  minimize average waiting time, since execution time is independent of schedule strategy

### response time

响应时间定义为从任务到达系统到首次运行的时间

# Schedule Strategies

## FIFO

we have [[convey effect]].

## SJF (shortest job first)
optimal if job arrive at the same time.

## STCF (Shortest Time-to-Completion First)
job can arrive at different time.

we need a [[preemptive scheduling]] method.

每当新工作进入系统时，它就会确定剩余工作和新工作中，谁的剩余时间最少，然后调度该工作



## RR (Round-Robin)

RR在一个时间片（time slice，有时称为调度量子，scheduling quantum）内运行一个工作，然后切换到运行队列中的下一个任务，而不是运行一个任务直到结束。它反复执行，直到所有任务完成。因此，RR有时被称为时间切片（time-slicing)

如果周转时间是我们的指标，那么RR确实是最糟糕的策略之一。直观地说，这应该是有意义的：RR所做的正是延伸每个工作，只运行每个工作一小段时间，就转向下一个工作。因为周转时间只关心作业何时完成，RR几乎是最差的，在很多情况下甚至比简单的FIFO更差。

## MLFQ (Multi-level Feedback Queue)

> 没有工作长度的先验（priori）知识，如何设计一个能同时减少响应时间和周转时间的调度程序?

![[Pasted image 20210812210011.png]]

- multi level priority queue
- select high priority task, round-robin amid the same queue
- collect execution statistics to update task priority.


> intuition: 如果不知道工作是短工作还是长工作，那么就在开始的时候假设其是短工作，并赋予最高优先级。如果确实是短工作，则很快会执行完毕，否则将被慢慢移入低优先级队列，而这时该工作也被认为是长工作了。通过这种方式，MLFQ近似于SJF。

1. (create) new task set to highest priority
2. (down grade)
	1. 工作用完整个时间片后，降低其优先级（移入下一个队列)
	2. 如果工作在其时间片以内主动释放CPU，则优先级不变
3. 一旦工作用完了其在某一层中的时间配额（无论中间主动放弃了多少次CPU），就降低其优先级（移入低一级队列）**(for malware)**
4. 经过一段时间S，就将系统中所有工作重新加入最高优先级队列 **(expensive task starvation)**



## proportional-share

