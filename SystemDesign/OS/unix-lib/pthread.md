# create a thread

```c
#include <pthread.h>

// succ ret 0
int pthread_create(
	pthread_t *thread, 								// out-param, init a thread struct serves as an ID
	const pthread_attr_t *attr,				     // attributes control thread creation 
	void *(*start_routine) (void *), 
	void *arg);

// Compile and link with -pthread.
```

## pthread attributes

```c
 #include <pthread.h>

int pthread_attr_init(pthread_attr_t *attr);
int pthread_attr_destroy(pthread_attr_t *attr);
```

## terminate
```c
int pthread_join(pthread_t  thread, void **  retval);
```


# locks

```c
```