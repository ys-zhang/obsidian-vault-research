[The Saturation Effect in Fuzzing – Embedded in Academia (regehr.org)](https://blog.regehr.org/archives/1796)

#todo 
# Problem & Observation

Here’s something we have seen happen many times:

-   We apply a fuzzer to some non-trivial system under test (SUT), and initially it finds a lot of bugs.
-   As these bugs are fixed, the SUT sort of becomes immune to this fuzzer: the number of new bugs found by the fuzzer drops off, eventually approaching zero.
-   Subsequently, a different fuzzer, applied to the same system, finds a lot of bugs.


What is going on with the first fuzzer?
1. The frequency at which the bugs in a system are triggered by any given source of stochastic inputs follows a _power law distribution_. 
2. Some bugs are out of reach for a given fuzzer even given an infinite amount of CPU time.