treat the performance of the system as a random variable.

[[jitter]] or **performance percentile** is critical to user experience.

`p95`: 95% percentile of the performance

> Even if you make the calls in parallel, the end-user request still needs to wait for the slowest of the parallel calls to complete.
> Even if only a small percentage of backend calls are slow, the chance of getting a slow call increases if an end-user request requires multiple back‐ end calls, and so a higher proportion of end-user requests end up being slow.


# Monitoring Percentile or distribution of performance

- [[forward decay]]
- [[t-digest]]
- [[HdrHistrogram]]


Graham Cormode, Vladislav Shkapenyuk, Divesh Srivastava, and Bojian Xu: “Forward Decay: A Practical Time Decay Model for Streaming Systems,” at 25th IEEE International Conference on Data Engineering (ICDE), March 2009.

Ted Dunning and Otmar Ertl: “Computing Extremely Accurate Quantiles Using t-Digests,” github.com, March 2014.

Gil Tene: “HdrHistogram,” hdrhistogram.org
