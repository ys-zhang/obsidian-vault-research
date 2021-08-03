Three views of fault tolerance: **the hardware view, the software view, and the global (holistic) view**.

# Definitions

**System availability.** The fraction of the offered load that is processed with acceptable response times.

**Availability class.** The number of leading nines in the availability figure for a system or module.

| System Type             | Unavailability (min/yr) | Availability | Class |
| ----------------------- | ----------------------- | ------------ | ----- |
| Unmanaged               | 52,560                  | 90%          | 1     |
| Managed                 | 5,256                   | 99%          | 2     |
| Well-managed            | 526                     | 99.9%        | 3     |
| Fault-tolerant          | 53                      | 99.99%       | 4     |
| High-availability       | 5                       | 99.999%      | 5     |
| Very-high-availability  | .5                      | 99.9999%     |       |
| Ultra-high-availability | .05                     | 99.99999%             |       |

A **failure** occurs because of an **error**, or a defect in the module. The cause of the **error** is a **fault**. The time between the occurrence of the **error** and the resulting **failure** is the **error latency**. When the **error** causes a **failure**, it becomes **effective**; before that, the failure is **latent**.

![[Pasted image 20210731170201.png]]

![[Pasted image 20210731170226.png]]

**Module reliability** measures the time from an initial instant to the next failure event. Reliability is statistically quantified as **mean-time-to-failure (MTTF)**;

**Service interruption** is statistically quantified as **mean-time-to-repair (MTTR).**

**Module availability** measure the ratio of service-accomplishment to elapsed time.

$$\frac{MTTF}{MTTF + MTTR}$$

**Faults can be hard or soft.** A module with a hard fault will not function correctly—it will continue with a high probability of failing—until it is repaired. A module with a soft fault appears to be repaired after the failure. Soft faults are also known as transient or intermittent faults.


# Algorithms

## Old master-new master (data replication)

the batch of `transaction`s accumulated during the day, week, or month was applied to the old `master file`, producing a new `master file`.

To protect against loss or damage of the `transaction file` (typically a deck of cards), or the `master file` (typically a tape), an archive copy of each was maintained.

Copies of the old masters were retained for five days, then week-old, month-old, and year-old copies were kept. In addition, all transaction inputs were kept. With this data, any old state could be reconstructed.

![[Pasted image 20210731170321.png]]


## valid construction and error correction.

**Validation** can remove errors during the construction process, thus ensuring that the constructed module conforms to the specified module.

**Error correction** reduces failures by using redundancy to tolerate faults.

**Error masking** uses redundant information to deliver the correct service and to construct a correct new state.

**Error recovery** denies the requested service and sets the module to an error-free state.

- backward error recovery: snapshot
- forward error recovery

### ECC: Error correcting codes

## Failfast

A module is **failfast** if it stops execution when it detects a fault (stops when it fails), and if it has a small fault latency (the fail and the fast). The term **failstop** is sometimes used to mean the same thing.

> Failfast behavior is important because latent errors can turn a single fault into a cascade of faults when the recovery system tries to use the latent faulty components. Failfast minimizes fault latency and so minimizes latent faults.

> The goal is to start with ordinary hardware, organize it into failfast hardware and software modules, and build up a system (a super module) that has no faults and, accordingly, is a highly available system (module). This goal can be approached with the controlled use of redundancy and with techniques that allow the super-module to mask, or hide, the failures of its component modules.

### N-PLEX

The simplest design, called **pairing** or **duplexing**, connects the inputs and outputs of _two modules_ to a comparator that _stops if the module outputs disagree_; this is the failfast aspect of the design.

Although a pair fails about twice as often as a single module, a failfast module gives clean failure semantics; it also reduces the number of cases fault-tolerant software needs to deal with by converting all failures to a very simple class: stopping.

### Failvote

![[Pasted image 20210731170518.png]]

**Failvote** The voter requires that a majority of the modules be available. If there is no majority, the voter stops

### Failfast(voting)

Similar to failvote except the system senses which modules are available and uses the majority of the available modules.

### Supermodule

Naturally, a system with multiple hard disk drives is expected to function with only one working disk (use voting when multiple disks are working/available, but still work even when only one is available)

> The prerequisition of these modes is failfast, i.e. there is no byzantine failure only crash failure.

with repair

$$P=\frac{n}{MTTF}(\frac{MTTR}{MRRF})^{n-1}$$


## Software faults

**N-version programming**. Write the program n times, test each program carefully, and then operate all n programs in parallel, taking a majority vote for each answer.

**Transactions**. Write each program as an ACID state transformation with consistency checks. At the end of the transaction, if the consistency checks are not met, abort the transaction and restart.

**Process pairing.** Primary process does all the work until it fails. The second process (backup) takes over the primary and continues.

-   Checkpoint-restart: The primary records its state on a second storage module.
-   Checkpoint messages: The primary sends its state changes as messages to the backup.
-   Persistent: backup restarts in the null state and lets Transaction mechanism to clean up all uncommitted transactions.

1. A **Heisenbug** is a transient software error (a soft software error) that only appears occasionally and is related to timing or overload.

2. **Bohrbugs**, like the Bohr atom, are good, solid things with deterministic behavior.

![[Pasted image 20210731170632.png]]

> Heisenbug proponents suggest crashing the system and restarting at the first sign of trouble; this is the failfast approach. It appears to make things worse, since the system will be crashing all the time, and the database and network will be corrupted when the system is restarted. This is where transactions come in.

> The programming style of failfast software designs is called defensive programming by analogy with the defensive automobile driving style advocated by traffic-safety experts. Defensive programming advocates that every software module check all its inputs and raise an exception if the inputs are incorrect.