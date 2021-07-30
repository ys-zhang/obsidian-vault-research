#program-test #fuzzing 

>It is not entirely blackbox because [[American Fuzzy Lop (AFL)]] leverages at least _some_ program analysis.
It is not entirely whitebox either because AFL does not build on heavyweight program analysis or constraint solving.
Instead, AFL uses lightweight program instrumentation to glean some information about the (branch) coverage of a generated input. If a generated input increases coverage, it is added to the seed corpus for further fuzzing.

```python
def coverage_based_greybox_fuzzing(S: "seed corpus"):
	S_crash = set()
	while True:
		s = choose_next(S)       # Search Strategy
		p = assign_energy(s)     # Power  Schedule
		for i in range(p):
			s0 = mutate_input(s)
			if crashed(s0):
				S_crash.add(s0)
			elif is_interesting(s0):
				S.add(s0)
	return S_crash
```

## instrument a program

AFL injects a piece of code right after every *conditional jump* instruction.

When executed, this so-called **trampoline** assigns the exercised branch a *unique identifier* and increments a *counter* that is associated with this branch.

## Mutator and Seed

## Power Schedules
A **power schedule** distributes the precious fuzzing time among the seeds in the population.

### exponential power schedule
$$ e(s)=\frac{1}{f(p(s))^a} $$

- $p(s)$ is the `uid` of the path exercised by $s$
- $f(p)$ returns the number of times the path $p$ is exercised by generated inputs, and
- $a$ is a exponent  


# Implementations

- [[American Fuzzy Lop (AFL)]]
