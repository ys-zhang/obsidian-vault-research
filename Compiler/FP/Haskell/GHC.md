#GHC 
# Memory

A Heap cell/node in GHC is value of type `StgClosure` declared in `rts/include/storage/Closures.h`

```c

// ==========================================================
// rts/include/Types.h
// ==========================================================
typedef struct StgClosure_   StgClosure;
typedef struct StgInfoTable_ StgInfoTable;

// ==========================================================
// rts/include/storage/Closures.h
// ==========================================================
typedef struct StgClosure_ {
    StgHeader   header;
    struct StgClosure_ *payload[];
} *StgClosurePtr; // StgClosure defined in rts/Types.h

typedef struct {
    const StgInfoTable* info;
#if defined(PROFILING)
    StgProfHeader       prof;
#endif
} StgHeader;

// ==========================================================
// rts/include/storage/InfoTables.h
// ==========================================================
typedef struct StgInfoTable_ {
#if !defined(TABLES_NEXT_TO_CODE)
    StgFunPtr       entry;      /* pointer to the entry code */
#endif
#if defined(PROFILING)
    StgProfInfo     prof;
#endif
    StgClosureInfo  layout;     /* closure layout info (one word) */
    StgHalfWord     type;       /* closure type */
    StgSRTField     srt;
#if defined(TABLES_NEXT_TO_CODE)
    StgCode         code[];
#endif
} *StgInfoTablePtr; // StgInfoTable defined in rts/Types.h
```


## Reference 
1. [GHC comments: heap objects](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects)
2. [[cs240h-memory]]

