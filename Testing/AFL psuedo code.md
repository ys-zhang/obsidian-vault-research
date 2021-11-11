 ```python
def fuzz(prog_input):
    y_obv = {}      # observed bitmaps
    x_obv = {}
    samples = {}
    
    # collect initial samples
    ...

    model = gen_model(samples)

    while True:
        x = gen_input(10000)
		
		havoc state:
			for (....) {
				generate 1 input
				fuzz the input
				collect time cost
			}
			if time cost < t:
				goto havoc
			goto next stage
		
        y_hat = predict(model, x)  100ms 10000 1-2 seconds
        similarity = compare(y_hat, y_obv)
        if similarity > threshold and roll_dice() < threshold:
            continue

        y = run_target(x)
        y_obv.add(y), x_obv.add(x)
        samples.add(x, y)
        if len(samples) >= N:
            try_update_model(model, y_obv, x_obv, samples)
            samples.clear()


def try_update_model(model, y_obv, x_obv, samples):

    n_x = count_x_longer_than_model_input(model_input_size, x_obv)

    if n_x / len(x_obv) > threshold:
        update(model, samples)
        return
    
    n_y = count_y_longer_than_model_output(model_output_size, y_obv)
    if n_y / len(y_obv) > threshold:
        update(model, samples)
        return
    
    similarity = compare(samples.y, model_training_samples.y)
    if similarity < threshold:
        update(model, samples)
        return
```


$$
	\sum_{y \in y_{obv}} cov(\hat{y}, y)
$$


Agree with Toby, that the bar to passing isn't as high as you might think. And like Thuan said wellbeing is super important. More than studies. And on the topic, many students do find CAPS very useful. Just like seeing a GP for a cold. They can even just provide a good person to talk to: https://services.unimelb.edu.au/counsel/home

reach the coordinator and cc Thuan



```python

def fuzz_loop(executor: Executor, manager: ExecutorManager
              state: FuzzerState, stages: FuzzerStages):
  while 1:
    timer.start()
    fuzz_one(executor, manager, state, stages)
    timer.end()
    # report stats
    if timer.timeout():
      manager.fire_update_state(state, curr)
      if COLLECT_PERF:
        state.update_perf()
        manager.fire_update_perf(state.execs, state.intro_specs,
                                 timer.dur)

def fuzz_one(executor: Executor, manager: ExecutorManager
             state: FuzzerState, stages: FuzzerStages):
  # schedule
  with state.intro_specs.schedule_timer():
    input_idx = fuzzer.scheduler.next(state)
  
  # perform stages
  #   mutations, calibrations
  with state.intro_specs.stages_timer():
    stages.perform_all(executor, state, manager, input_idx)
  
  # report to manager
  with state.intro_specs.stages_timer():
    manager.process(state, executor)


def execute_input(state, executor, manager, input_):
  """
  Use executor to perform a run, in Fuzzer 
  """
  # pre-execution
  executor.observer.pre_exec_all(state, input_)
  # execution
  executor.run_target(state, manager, input_)
  # post_execution
  executor.post_exec_all(state, input_)
    

def process_execution(stage, manager, input_,
                      observers, exit_kind):
  """ 
  This is done after run execute_input, in Fuzzer
  """
  objective: Feedback
  feedback:  Feedback
  scheduler: Scheduler
  
  # solution
  if objective.is_interesting(state, manager, input_,
                              observers, exit_kind):
    objective.append_metadata(state, input_, exit_kind)
    feedback.discard_metadata(state, input_, exit_kind)
    return IS_SOLUTION
  
  # add to corpus
  elif feedback.is_interesting(state, manager, input_,
                               observers, exit_kind):
    objective.discard_metadata(state, input_, exit_kind)
    feedback.append_metadata(state, input_, exit_kind)
    input_idx = state.corpus.add(input_)
    schedular.on_add(state, input_idx)
    
    # fire events to observers
    manager.fire_new_testcase(state, input_, oberver)
    return IS_CORPUS
  
  # discard
  else:
    objective.discard_metadata(state, input_, exit_kind)
    feedback.discard_metadata(state, input_, exit_kind)
    # donot report to observers
    return DISCARD, 0
    
    
    
    
    
```




```rust
fn fuzz_one(
    &mut self,
    stages: &mut ST,
    executor: &mut E,
    state: &mut S,
    manager: &mut EM,
) -> Result<usize, Error> {
    // Init timer for scheduler
    #[cfg(feature = "introspection")]
    state.introspection_stats_mut().start_timer();

    // Get the next index from the scheduler
    let idx = self.scheduler.next(state)?;

    // Mark the elapsed time for the scheduler
    #[cfg(feature = "introspection")]
    state.introspection_stats_mut().mark_scheduler_time();

    // Mark the elapsed time for the scheduler
    #[cfg(feature = "introspection")]
    state.introspection_stats_mut().reset_stage_index();

    // Execute all stages
    stages.perform_all(self, executor, state, manager, idx)?;

    // Init timer for manager
    #[cfg(feature = "introspection")]
    state.introspection_stats_mut().start_timer();

    // Execute the manager
    manager.process(self, state, executor)?;

    // Mark the elapsed time for the manager
    #[cfg(feature = "introspection")]
    state.introspection_stats_mut().mark_manager_time();

    Ok(idx)
}
```


```
fuzzer.fuzz_loop 
-> (
      fuzzer.fuzz_one() ========>
      manager.fire_update_state()
      manager.fire_update_perf()
   )
========>(
            fuzzer.scheduler.next(state)
            stages.perform_all(fuzzer, state) ------>
         ) 
-> stage.perform()
-> MutationalStage.perform_mutational
-> (
      mutator.mutate()
      fuzzer.evaluate_input()  ------>
      mutator.post_exec()
   )
-> 
```