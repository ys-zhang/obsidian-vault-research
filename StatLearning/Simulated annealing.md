#algorithm-search #machine-learning

## Simulated Annealing

>In metallurgy, annealing is the process used to temper or harden metals and glass by heating them to a high temperature and then gradually cooling them, thus allowing the material to reach a low energy crystalline state.

This notion of ***slow cooling*** implemented in the simulated annealing algorithm is interpreted as a ***slow decrease in the probability of accepting worse solutions*** as the solution space is explored.

The temperature progressively decreases from an initial positive value to zero.

randomly generates a move from the current state:

1.  if the move improves the situation, accept it.
2.  else: accept with some probability $p \propto \exp{-\frac{\Delta E}{T}}$ where
    -   $\Delta E = \textrm{next\_state.value - curr\_state.value}$
    -   $T = \textrm{schedule}(t)$

```python
def simulated_annealing(problem, schedule) -> "solution state":
	""" 
	search for solution minimize energy E

	@param schedule :: current_time -> temperature 
	"""
	current_state = initail_state(problem)
	for t in range("inf"):
		T = schedule(t)        # type: temperature
		# keep searching while T > 0
		if T == 0:
			return current_state
		next_state = select_successor_randomly(problem, current_state)
		delta_E = next_state.value - current_state.value
		if delta_E > 0:
			current_state = next_state
        else if rand.random() <= exp(delta_E / T):
			current_state = next_state
```