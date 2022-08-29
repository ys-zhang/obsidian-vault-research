#model-checking 
#concurrency 
#DSL


# STUDY ON FACILITATING THE VARIATION OF SCHEDULERS IN MODEL CHECKING

In this research, we aim at verifying concurrent applications (systems) which run on OSs using model checking techniques.

```
+-------+
|Process|-----------+
+-------+           |
                    |+-------> Controls the Execution 
+---------+         |
|Scheduler|---------+
+---------+
```

Verification needs a __specification language__ to describe the properties/behaviour of the system. 
To increase the indirection the author proposes to modelling _processes_ and _schedulers_ separately.
