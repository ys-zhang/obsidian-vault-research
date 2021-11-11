see http://lcamtuf.coredump.cx/afl/
#program-test  #fuzzing

AFL is a _mutation-based fuzzer_. Meaning, AFL generates new inputs by slightly modifying a seed input (i.e., mutation), or by joining the first half of one input with the second half of another (i.e., splicing).

AFL is also a _greybox fuzzer_ (not blackbox nor whitebox). Meaning, AFL leverages coverage-feedback to learn how to reach deeper into the program. It is not entirely blackbox because AFL leverages at least _some_ program analysis.

AFL’s instrumentation captures [[basic block]] transitions, along with coarse branch-taken hit counts. CGF uses the coverage information to decide which generated inputs to retain for fuzzing, which input to fuzz next and for how long.

```
1.  Download and install AFL fuzzer
2.  export WORKDIR=$(pwd)
3.  git clone https://github.com/google/AFL
4.  cd AFL
5.  make clean all; cd llvm-mode
6.  LLVM_CONFIG=llvm-config-6.0 make
7.  cd $WORKDIR/AFL
8.  export AFL=$(pwd)
9.  export AFL_PATH=$(pwd)
10.  export PATH=$PATH:$AFL

12.  Download and compile LibPNG
13.  git clone https://github.com/glennrp/libpng.git libpng-afl 
14.  cd libpng-afl
15. c
16.  CC=afl-clang-fast ./configure --disable-shared
17.  make clean all
18.  export LIBPNG=$(pwd)

3.  Run fuzzing
4.  cd $LIBPNG
5.  afl-fuzz -i seed_corpus_folder -o output_folder ./pngimage @@
```



# Screen 
![[Pasted image 20210815161038.png]]

[AFL/status_screen.txt at master · google/AFL · GitHub](https://github.com/google/AFL/blob/master/docs/status_screen.txt)

```
// overall results
+-----------------------+
| cycles done : 0        |     // count of queue passes done so far
| total paths : 2095    |     // the number of test cases ("paths") discovered so far
| uniq crashes : 0      |
| uniq hangs : 19       |
+-----------------------+

// cycle progress
// how far along the fuzzer is with the current queue cycle
+-------------------------------------+
| now processing : 1296 (61.86%) |  
| paths timed out : 0 (0.00%)        |
+-------------------------------------+


// map coverage
// 1. map density how many branch tuples we have already hit, in proportion to how much the bitmap can hold.
// 2. The other line deals with the variability in tuple hit counts seen in the binary.
+--------------------------------------+
| map density : 10.15% / 29.07% |   // curent input / entier input
| count coverage : 4.03 bits/tuple |
+--------------------------------------+
```

# References
- [GitHub - antonio-morales/Fuzzing101](https://github.com/antonio-morales/Fuzzing101)
- [GitHub - google/AFL: american fuzzy lop - a security-oriented fuzzer](https://github.com/google/AFL)



# code

```c

/* Configure shared memory and virgin_bits. This is called at startup. */

EXP_ST u8  virgin_bits[MAP_SIZE],       /* Regions yet untouched by fuzzing */
                     virgin_tmout[MAP_SIZE],    /* Bits we haven't seen in tmouts   */
           			 virgin_crash[MAP_SIZE];     /* Bits we haven't seen in crashes  */

EXP_ST void setup_shm(void) {

  u8* shm_str;
	
  if (!in_bitmap) memset(virgin_bits, 255, MAP_SIZE);

  memset(virgin_tmout, 255, MAP_SIZE);
  memset(virgin_crash, 255, MAP_SIZE);

  shm_id = shmget(IPC_PRIVATE, MAP_SIZE, IPC_CREAT | IPC_EXCL | 0600);

  if (shm_id < 0) PFATAL("shmget() failed");

  atexit(remove_shm);

  shm_str = alloc_printf("%d", shm_id);

  /* If somebody is asking us to fuzz instrumented binaries in dumb mode,
     we don't want them to detect instrumentation, since we won't be sending
     fork server commands. This should be replaced with better auto-detection
     later on, perhaps? */

  if (!dumb_mode) setenv(SHM_ENV_VAR, shm_str, 1);

  ck_free(shm_str);

  trace_bits = shmat(shm_id, NULL, 0);
  
  if (trace_bits == (void *)-1) PFATAL("shmat() failed");

}

```