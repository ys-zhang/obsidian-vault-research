# The Data Model of git

## Blob, Tree and Snapshot

> Git models the _history of a collection of files and folders_ within some top-level directory as a series of **_snapshots_**.

| Git terminology | Meaning                                  |
| --------------- | ---------------------------------------- |
| blob            | file                                     |
| tree            | directory                                |
| snapshot/object | the top level tree that is being tracked |
| object          | blob, tree or commit                     |
| reference       | pointers to commits                      |
| repository      | objects and references                   |
|                 |                                          |

```
<root> (tree)
|
+- foo (tree)
|  |
|  + bar.txt (blob, contents = "hello world")
|
+- baz.txt (blob, contents = "git is wonderful")
```

```shell
# show object
git cat-file -p <object-hash>;
```

## History as DAG

> In Git, a **_history_** is a _directed acyclic graph_ (DAG) of _snapshots_.

![[Git history.excalidraw]]
1. Each node in the DAG is a **snapshot** or **commit**;
2. **Commits** are _immutable_, _edits_ to _commits_ history are actually creating new commits;
3. **References** are _mutable_ pointers point to _commits_;

## Staging Area

**Staging area** is orthogonal to the data model.

## Pseudo Code
```typescript
// a file is a bunch of bytes
type blob = byte[];
// a directory contains named files and directories
type tree = Map<string, tree | blob>;
// a commit has parents, metadata, and the top-level tree
interface commit {
  parents: commit[];
  author: string;
  message: string;
  snapshot: tree;
}
type object = blob | tree | commit;

var OBJECTS = new Map<string, object>();
function store(obj: object) {
  let id: string = sha1(obj);
  OBJECTS.set(id, obj);  
}
function load(id: string) {
  return OBJECTS[id];
}

var REFERENCES = new Map<string, string>;
function update_reference(name, id){
  references[name] = id;
}
function read_reference(name){
  return references[name]
}
function load_reference(name_or_id){
  if name_or_id in references{
    return load(references[name_or_id]);
  } else {
    return load(name_or_id);
  }
}
    
```


# Commands

## Rebase



# Reference

- [[Git-Cheatsheet.pdf]]
- [MIT Git Lecture](https://missing.csail.mit.edu/2020/version-control/)
- [Stack overflow: pretty git branch graphs](https://stackoverflow.com/questions/1057564/pretty-git-branch-graphs)
- [Book: Pro Git](https://git-scm.com/book/en/v2)
