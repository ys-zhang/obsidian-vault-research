see http://lcamtuf.coredump.cx/afl/
#program-test  #fuzzing

AFL is a _mutation-based fuzzer_. Meaning, AFL generates new inputs by slightly modifying a seed input (i.e., mutation), or by joining the first half of one input with the second half of another (i.e., splicing).

AFL is also a _greybox fuzzer_ (not blackbox nor whitebox). Meaning, AFL leverages coverage-feedback to learn how to reach deeper into the program. It is not entirely blackbox because AFL leverages at least _some_ program analysis.

AFL’s instrumentation captures [[basic block]] transitions, along with coarse branch-taken hit counts. CGF uses the coverage information to decide which generated inputs to retain for fuzzing, which input to fuzz next and for how long.