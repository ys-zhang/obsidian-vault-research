- The `docker build` command builds an image from a `Dockerfile` and a _context_.
- _context_    can be specified by
    - `PATH`:  directory on _local filesystem_. `docker build $path` 
    - `URL`:    a _git repository_ location `docker build -t shykes/myapp`
    - `Dockerfile`:  `docker build -f $path_to_Dockerfile`


# Format of Dockerfile

```dockerfile
# Comment
# INSTRUCTION arguments

FROM [--platform=<platform>] <image>[:<tag>] [AS <name>]
```


