[AFLplusplus/LibAFL: Advanced Fuzzing Library - Slot your Fuzzer together in Rust! Scales across cores and machines. For Windows, Android, MacOS, Linux, no_std, ... (github.com)](https://github.com/AFLplusplus/LibAFL)

[Architecture - The LibAFL Fuzzing Library (aflplus.plus)](https://aflplus.plus/libafl-book/design/architecture.html)

# Concepts

1. An **Observer**, or **Observation Channel**, is an entity that provides an information observed _during_ the execution of the program under test to the fuzzer. The information contained in the **Observer** is _not preserved_ across executions. (see [[#The Observer trait]])
2. **Executor** is the entity that defines not only _how to execute the target_, but _all the volatile operations_ that are related to just a _single_ run of the target. (see [[#The Executor trait]])
3. The **Feedback** is an entity that classifies the _outcome_ of an execution of the program under test as _interesting or not_. (see [[#The Feedback and FeedbackState traits]])
4. **Input** of the target.
5. **Testcase** contains an **Input** and a set of related _metadata_ like execution time for instance.
6. **Corpus** is where **Testcase**s are stored.
7. **Mutator**
8. **Generator** is a component designed to generate an **Input** from scratch.
9. **Stage** is an entity that operates on a single Input got from the Corpus.

# Structure


##### The `Observer` trait

1. Holding the _volatile data_ connected with the _last_ execution of the target.
2. Can define some execution _hooks_ that are executed before and after each fuzz case. In these hooks, the observer _can modify the fuzzer's state_.



```rust

impl Evaluator for StdFuzzer {
  fn run_target(&mut self, fuzzer: &mut F, state: &mut S,
                mgr: &mut EM, input &I) -> Result<ExitKind, Error>
  {
    
  }
}

```

##### The `Executor` trait

1. Inform the target about the _input_ that the fuzzer wants to use in the run, writing to a memory location for instance or passing it as a parameter to the harness function.
2. Implement `HasObservers` if wants to hold a set of `Observer`s.

Examples:
  - `InProcessExecutor`: targets a harness function providing in-process crash detection.
  - `ForkServerExecutor`: an AFL-like mechanism to spawn child processes to fuzz.

> A common pattern when creating an `Executor` is _wrapping_ an existing one. (`TimeoutExecutor`)

##### The `Feedback` and `FeedbackState` traits

`Feedback` implement functors that, given the state of the observers from the last execution, tells if the execution was interesting.

`FeedbackState`is the state of the data that the feedback wants to persist in the fuzzers's state, e.g. `virgin_edges`

##### The `Corpus` and `CorpusScheduler` traits


# Architecture

1. _Observer_ is run pre/post on each execution of the target
2. After observation, _Feedback_ judges whether to mark as solution and add to seed corpus and changes _metadata in testcase_(which wraps input), and _notifies the scheduler__ 


```python

def fuzz_loop():
  state: a_global_fuzzer_state
  scheduler: choosing_next_seed
  stages: a_list_of_fuzzing_stages
  
  while 1:
    seed = scheduler.next(state)
    reset_current_stage(state)  
    
    """ perform each stage on the seed """
    for stage in stages:
      
      """ mutate stage """
      power = get_stage_max(stage)
      for _ in range(power):
        exec_kind = target_input = mutate_input(seed)
        observer.observe_pre_exec(state, target_input)
        run_target(state, target_input)
        observer.observe_post_exec(state, target_input)
        
        """ExecutionProcessor by StdFuzzer.process_execution"""
        test_case = new_from(target_input)
        if objective.is_interesting(stage, manager, input, 
                                    observer, exec_kind):
          # add meta to the target input
          objective.append_metadata(state, test_case)
          solution_corpus.add(test_case)
          manager.fire_event()
        else:
          objective.discard_metadata(state, target_input)
        
        if feedback.is_interesting(stage, manager, input, 
                                   observer, exec_kind):
          feedback.append_metadata(state, test_case)
          seed_corpus.add(test_case)
          scheduler.on_add(state, input_idx)
          manager.fire_event()
        else:
          objective.discard_metadata(state, target_input)
          
          
```