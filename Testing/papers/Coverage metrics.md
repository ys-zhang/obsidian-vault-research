#coverage-metric

# Be Sensitive and Collaborative:  Analyzing Impact of Coverage Metrics in Greybox Fuzzing


# Problem

1.    How different _coverage metrics_ could actually affect the fuzzing results in practice?
2.    Whether there exists one coverage metric that is _superior_ to all the other metrics?


# Types of coverage metric

## Branch coverage
 
 AFL identifies a branch as
   
       (prev_block, cur_block)
 In practice, branch coverage is usually measured by hashing this tuple (as key) into a hash table (e.g., a hit_count map). 
 
       block_trans = (prev_block << 1) ⊕ cur_block   
example:
```c
// afl-compiler-rt.o.c

void __afl_trace(const u32 x) {

  PREV_LOC_T prev = __afl_prev_loc[0];
  __afl_prev_loc[0] = (x >> 1);

  u8 *p = &__afl_area_ptr[prev ^ x];

#if 1                                      /* enable for neverZero feature. */
  #if __GNUC__
  u8 c = __builtin_add_overflow(*p, 1, p);
  *p += c;
  #else
  *p += 1 + ((u8)(1 + *p) == 0);    /*** @note: (zys) avoid overflow */
  #endif
#else
  ++*p;
#endif

  return;

}
```


## n-gram coverage

A generalization of [[#Branch coverage]], considering longer execution history.

       prev_block_trans = ( block_trans[1] ⊕···⊕ block_trans[n-1] )
       block_trans[n] = (prev_block_trans << 1) ⊕ cur_block
       

If $n$ is too small, it might be almost the same as branch coverage. If $n$ is too large, it may cause seed explosion (a similar phenomenon as path explosion)


##   Context-Sensitive Branch Coverage

Calling context is another important piece of information that can be incorporated as part of the coverage  
metric, which _allows a fuzzer to distinguish the same code executed with different data_.

    (call_stack, prev_block, curr_block)


One variation is to define a calling context $coll_ctx$  as _a sequence of program locations_ where function calls are made in order.

$$
call\_ctx = \begin{cases}
 0     & \textrm{initial value} \\
 call\_ctx \; \oplus \; call\_next\_insn  & \textrm{if call} \\
 call\_ctx \; \oplus \; ret\_to\_insn  & \textrm{if ret} \\
\end{cases}
$$

##  Memory-Access-Aware Branch Coverage 
 see [[The Use of Likely Invariants as Feedback for Fuzzers]].

