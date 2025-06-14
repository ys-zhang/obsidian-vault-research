#code-reading #Rust 

# Video Notes


## VFS 

>[!question]
What the `vfs` crate for, why not use `std::fs::reading_to_string("foo.fs")` but a new virtual file system?
>  - [[non-repeatable read]]: _rust-analyzer forget things_, IDE generally uses much more memory than compilers, thus to opt mem usage, rust-analyzer will forget things. Thus rereading is not rare.
>  - _Platform agnostic path_: file path are os dependent.
>  - _Multi-filesystem_. write code on Win but run rust-analyser on Linux.

```rust
/// This is how RA use to refer to some file
pub struct FileId (pub u32)

/// Vfs is actually a snapshot of 
pub struct Vfs {
    interner: PathInterner,
    /// a flat array stores all file data/content
    /// if the content is None, then the file does
    /// not exist, i.e., deleted.
    data: Vec<Option<Vec<u8>>>,
    /// what file changed?
    /// is it modified or deleted?
    /// 
    /// these changes are kept for components 
    ///   to figure out which part needs to be 
    ///   rerun
    changes: Vec<ChangedFile>,
}

impl Vfs {
  // mk changes to the Vfs snapshot
  pub fn set_file_contents(
    &mut self, 
    path: VfsPath, 
    contents: Option<Vec<u8>) -> bool {
    ...
  }
}

pub enum VfsPath {
  PathBuf(AbsPathBuf),
  VirtualPath(String)  // maybe remote, in some db or in memory
}

```

>[!question]
Why use `FileId` but not just `VsfPath`?
>  - Force all file access through file system, no one can accidentally use `VfsPath` to read data from `std::fs`.
>  - A `u32` is more compact

### Loader

The task of `vfs::loader` is to watch some files and once they are changed, inform and change the `Vfs` snapshot.

```rust

/// config of what files shall be loaded
/// and whether it shall be watched
/// config can change over time
pub struct Config {
    /// Version number to associate progress 
    /// updates to the right config version.
    pub version: u32,
    /// Set of initially loaded files.
    pub load: Vec<Entry>,
    /// Index of watched entries in `load`.
    /// If a path in a watched entry is modified,
    /// the [`Handle`] should notify it.
    pub watch: Vec<usize>,
}

/// Type that will receive [`Messages`](Message) from a [`Handle`].
pub type Sender = Box<dyn Fn(Message) + Send>;

/**************************************
  The real implementation is in crate:
     vfs-notify
 **************************************/
/// Interface for reading and watching files.
pub trait Handle: fmt::Debug {
    /// Spawn a new handle with the given `sender`.
    fn spawn(sender: Sender) -> Self
    where
        Self: Sized;

    /// Set this handle's configuration.
    fn set_config(&mut self, config: Config);

    /**************************************
     why invalidate:
         loader only watch for file changes
         on disk, however, edits in editors
         will not hit the disk before buffer
         is saved
     **************************************/

    /// The file's content at `path` has been modified, 
    /// and should be reloaded.
    fn invalidate(&mut self, path: AbsPathBuf);

    /// Load the content of the given file,
    /// returning [`None`] if it does not
    /// exists.
    fn load_sync(&mut self, path: &AbsPath) -> Option<Vec<u8>>;
}

```


>[!note] file watching
> OS provided interface are generally hard to guarantee some basic properties 
> a watch service needs, such as 
> 1. preserving order of modification events
> 2. eventually consistency of modification and last file states
> Thus its usually much easier to use some file watching programs dedicated to 
> handle these tasks, such as [watchman](https://facebook.github.io/watchman/)

### Anchored Path

This concept is also used to deal with the case when server and clients runs in different types of systems.

The problem here is that multiple clients may ask the server to process different files with same path string but located in different machines. Thus, absolute path no longer be able to serve as an identifier for files.


```rust
/// Path relative to a file.
///
/// Owned version of [`AnchoredPath`].
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct AnchoredPathBuf {
    /// File that this path is relative to.
    pub anchor: FileId,
    /// Path relative to `anchor`'s containing directory.
    pub path: String,
}

```


### FileSet 

`FileSet` allows us to split a VFS into several disjoint chunks, the idea here is to create a `FileSet` for each crate. This provides some level of isolation and prevents unnecessary analysis propagate into the whole VFS  


## `ide` crate


```rust
/// `AnalysisHost` stores the current state of the world.
#[derive(Debug)]
pub struct AnalysisHost {
    db: RootDatabase,
}

/// Analysis is a snapshot of a world state at a moment in time. 
/// It is the main entry point for asking semantic 
//  information about the world. When the world
/// state is advanced using `AnalysisHost::apply_change` method, 
/// all existing `Analysis` are canceled
/// (most method return `Err(Canceled)`).
#[derive(Debug)]
pub struct Analysis {
    db: salsa::Snapshot<RootDatabase>,
}

```

Basically `AnalysisHost` _owns_ the current state of the world, and is responsible react to its changes.  On the other hand, `Analysis` is an immutable reference points to some state of the world. This setup is for doing analysis concurrently.

When change happens, `AnalysisHost` will hold the changes and notify and wait all concurrent analysis tasks to finish, until then the changes are applied.

```rust
// this is just the mental model

loop {
  match fetch_changes() {
    Some(..) => {
      analysis_host.notify_all_analysis();
      wait(analysis_host.all_analysis_cancelled_or_finished());
      analysis_host.apply_changes();
      spawn_new_analysis_tasks();
    },
    None => {
      ...
      // do nothing
    }
  }
}
```
