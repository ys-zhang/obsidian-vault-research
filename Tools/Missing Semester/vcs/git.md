#version-control

Modern VCSs also let you easily (and often automatically) answer questions like:
- Who wrote this module?
- When was this particular line of this particular file edited? By whom? Why was it edited?
- Over the last 1000 revisions, when/why did a particular unit test stop working?

# 1 The Data Model of git

## 1.1 Concepts

### 1.1.1 Blob, Tree and Snapshot

> Git models the _history of a collection of files and folders_ within some top-level directory as a series of **_snapshots_**.

| Git terminology | Meaning                                  |
| --------------- | ---------------------------------------- |
| blob            | an object that is a file                 |
| tree            | an object that is a directory            |
| snapshot/commit | the top level tree that is being tracked |
| object          | blob, tree or commit                     |
| reference       | pointers to commits                      |
| repository      | objects and references                   |
|                 |                                          |

![[Pasted image 20241013220618.png]]

>[!example] git model
> ```
> root (tree)
> |
> +- foo (tree)
> |  |
> |  + bar.txt (blob, contents = "hello world")
> |
> +- baz.txt (blob, contents = "git is wonderful")
> ```

>[!tip] inspect object
> 1. use `{bash}git show` to inspect any object, suit best for _blob_
>     - `{bash}git show -s --pretty=raw HEAD`
> 2. use `{bash}git ls-tree` inspect a _tree_ (directory) 
> 3. use `{bash}git cat-file` to inspect a _tag_

>[!remark] loose and packed objects
> large number of small files hurts performance thus we store a collection of objects in one file in the OS's file system.
>
> see also 
>  - (porcelain) `git gc`
>  - (plumbing) `git repack`, `git count-objects` and `git prune`.

### 1.1.2 History as DAG

>[!def] git history
> In Git, a **_history_** is a _directed acyclic graph_ (DAG) of _snapshots_.

![[Git history.excalidraw|center 100%]]
1. Each node in the DAG is a **snapshot** or **commit**;
2. **Commits** are _immutable_, _edits_ to _commits_ history are actually creating new commits;
3. **References** are _mutable_ pointers point to _commits_;

>[!tip] view the history DAG
> - `{bash}git log --graph`
> - `{bash}man gitrevisions` shows how to refer to snapshots

>[!def] branch & branch head
> the word "_branch_" to mean a line of development, and "_branch head_" (or just "head") to mean a reference to the most recent commit on a branch.
> 
> However, when no confusion will result, we often just use the term "branch" both for branches and for branch heads.


>[!def] reachable
> we say that commit $X$ is "_reachable_" from commit $Y$ if commit $X$ is an ancestor of commit $Y$.
> $$ Child \longmapsto_\star Ancestor $$

see also [[#2.2.3 branch management]].

#### 1.1.2.1 remote-tracking branch 

Checking out a local branch from a remote-tracking branch automatically creates what is called a  “_tracking branch_” (and the branch it tracks is called an “_upstream branch_”). 

Tracking branches are local branches that have a direct relationship to a remote branch. If you’re on a tracking branch and type git pull, Git automatically knows which server to fetch from and which branch to merge in.

If you already have a local branch and want to set it to a remote branch you just pulled down, or want to change the upstream branch you’re tracking, you can use the `-u` or `--set-upstream-to` option to `{bash}git branch` to explicitly set it at any time.

```bash
git remote add <repo-name> <path-to-repo>
# set branches of remote to track
git remote set-branches <repo-name> <branch-list-to-track>
```


#### 1.1.2.2 rebase

rebase works by going to the common ancestor of the two branches (the one you’re on and the one you’re rebasing onto), getting the diff introduced by each commit of the branch you’re on[^patch], saving those diffs to temporary files, resetting the current branch to the same commit as the branch you are rebasing onto, and finally applying each change in turn.

[^patch]: `{bash} git diff > some.patch` produces a _patch_ file describes the delta of the 2 versions, which can be applied to one to get the other.

rebasing makes for a cleaner history. If you examine the log of a rebased branch, it looks like a linear history: it appears that all the work happened in series, even when it originally happened in parallel.

Often, you’ll do this to make sure _your commits apply cleanly on a remote branch_ — perhaps in a project to which you’re trying to contribute but that you don’t maintain. In this case, you’d do your work in a branch and then rebase your work onto origin/master when you were ready to submit your patches to the main project. That way, the maintainer doesn’t have to do any integration work — just a fast-forward or a clean apply.

>[!danger]
> Do not rebase commits that exist outside your repository and that people may have based work on.


>[!note] Rebase vs. Merge
> - One point of view on this is that your repository’s commit history is __a record of what actually happened__.
> - The opposing point of view is that the commit history is **the story of how your project was made**.
>
> People in the 2nd camp use tools like `rebase` and `filter-branch` to rewrite their commits before they’re merged into the mainline branch, telling the story in the way that’s best for future readers.




### 1.1.3 Four Areas

A conceptual file can live in 4 areas,
1. _working directory/tree_, is the folder where the repo lives in the os file system;
2. _staging area_;
3. _history/commit_ or _object store_;
4. _stash area_.

![[Pasted image 20241012110040.png]]
![[Pasted image 20241012110429.png]]

>[!tip] data flow of working-tree, index & object database
> - `git restore` uses object database to restore stage area (and working tree), see also [[#3.1 `git switch` and `git restore`]];
> - `git rm` deletes files in working tree and stage area.
> - `git reset` reverts either snapshot pointed by HEAD or the index from object database.

>[!danger] 
> the `{bash}git restore` by default touches the _working directory_, thus prefer `{bash}git reset` over `{bash}git restore`

>[!note] restore, reset and remove
> - `git rm --cached <file>`: removes the copy of the file from the index/staging-area, without touching the working tree copy. 
>   > The proposed next commit now _lacks_ the file. If the current commit _has_ the file, and you do in fact make a next commit at this point, the difference between the previous commit and the new commit is that the file is gone.
> - `git restore --staged <file>`: Git copies the file from the `HEAD` commit into the index, without touching the working tree copy. 
>   > The index copy and the `HEAD` copy now match, whether or not they matched before. A new commit made now will have the _same_ copy of the file as the current commit.
>   > If the current commit _lacks_ the file, this has the effect of _removing_ the file from the index. So _in this case_ it does the same thing as `git rm --cached`.
> - `git reset <file>`: this copies the `HEAD` version of the file to the index, just like `git restore --staged <file>`.

#### 1.1.3.1 Staging Area and the Index

> [!def] Staging Area
> To _keep a track of modifications or changes_ in the file we have to bring that changes to our **Staging Area** which we bring by using **Staging**.
>
> **Staging area** is orthogonal to the data model, and is implemented by the [git index](https://git.github.io/htmldocs/user-manual.html#the-index)

>[!def] Hunk
> **Hunk** means a piece of change.

![[git-staging.excalidraw|center 100%]]


>[!def] git index
> the _git index_ is also dubbed as _current directory cache_ or shorter _cache_.
>
> the purpose the index is to enable _fast comparisons btw the tree obj it caches and the working directory (the file-system)_.

#### 1.1.3.2 Stash Area

>[!def] 
>Stashing takes the dirty state of your working directory — that is, your modified tracked files^[untracked files can also be stashed with `--include-untracked` or `-u`] (_working tree_) and staged changes (_index_) — and saves it on a stack of unfinished changes that you can reapply at any time (even on a different branch).

- `{bash}git stash push/pop [[--] <path>]` 
  - `{bash}git stash push --include-untracked` or use the abbreviation `-u`.  By default, only the working tree under tracking is pushed to stash, this option enables pushing untracked files. 
  - `{bash}git stash push --keep-index` by default after `push`, git will roll back both the working tree and the index to `HEAD`, this option prevents rolling back the index.
  - `{bash}git stash push --patch`
- `{bash}git stash apply` apply a stash without pop
- `{bash}git stash branch <branch-name> [<stash-id>]`, this com­mand cre­ates a new branch at the HEAD where the stash was cre­at­ed, checks that branch out, applies the stash to it, and then deletes the stash.

### 1.1.4 submodule

> Git’s submodule support allows a repository to contain, as a subdirectory, a checkout of an _external project_.
>
> Submodules maintain their own identity; the submodule support just stores the _submodule repository location and commit ID_, so other developers who clone the containing project (_"super-project"_) can easily clone all the submodules at the same revision. 
>
> _Partial checkouts_ of the super-project are possible: you can tell Git to clone none, some or all of the submodules.

- data of submodules are stored in `.gitmodules` instead of `.git`

### 1.1.5 Other concepts

1. _bare repository_, is a repo that does not contain working-tree
  - `git init --bare` create a bare repo;
  - `git clone --bare`
2. _tag_ is a named reference of a specific snapshot; 
3. the _git directory_ (`.git`)^[check `man gitrepository-layout`]
  - file `.git/HEAD` always points to a ref in `.git/refs/heads` which is the current working branch;
  - `.git/objects` folder of the _object store_
  - `.git/refs` folder of the _ref table_
    - `.git/refs/heads` folder of ref names of _branches_;
    - `.git/refs/tags` folder of ref names of _light weight tags_.


>[!def] tag
>- Tag objects (created with `-a`, `-s`, or `-u`) are called _"annotated" tags_; they contain a creation date, the tagger name and e-mail, a tagging message, and an optional GnuPG signature. 
>- Whereas a _"lightweight" tag_ is simply a name for an object (usually a commit object).


>[!def] head detached mode
>the current state is said to be in _detached mode_ if `HEAD` do not refers to some branch's head but directly to some old commit/snapshot.
  
## 1.2 Pseudo Code

![[Pasted image 20241013220618.png]]

```haskell
{- cabal:
build-depends: 
  base, bytestring, containers, 
  lens, crypton
-}

{-# LANGUAGE GADTs, TemplateHaskell #-}
module Git where

import Control.Lens
import Control.Lens.TH(makeLenses)
import Crypto.Hash (Digest, hashWith, SHA1(..))
import Data.ByteString qualified as B
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | object name sha1 of the object   
type HashRef = Digest SHA1 
-- | a readable reference points to an object
type NameRef = Text        

data Ptr a where 
  BlobHash   :: HashRef -> Ptr Blob
  TreeHash   :: HashRef -> Ptr Tree
  CommitHash :: HashRef -> Ptr Commit
  BlobRef    :: NameRef -> Ptr Blob
  TreeRef    :: NameRef -> Ptr Tree
  CommitRef  :: NameRef -> Ptr Commit

-- | a file is a bunch of bytes
type Blob = B.ByteString;

-- | a directory contains named files and directories
-- view a tree by `git ls-tree`
data Tree = Dir  TreeItemInfo [Tree] 
          | File TreeItemInfo Blob 

data TreeItemInfo = MkTreeItemInfo 
  { hashName :: HashRef
  , fsName   :: Text
  , fsMode   :: Word  -- ^ 6 bytes
  }
 
-- | a commit has parents, metadata, 
-- and the top-level tree
data Commit = MkCommit 
  { parents   :: [Ptr Commit]
  -- ^ git allows multiple parents 
  , snapshot  :: Ptr Tree
  -- ^ top level tree
  , author    :: Text
  -- ^ the one who responsible for the change
  -- in other words, who write the code
  , committer :: Text
  -- ^ the one who created the commit
  -- in other words, who run the git commit command
  , message   :: Text
  }
  
-- | objects that can be serialised on disc
-- these are also called _loose object_
data Obj 
  = Blob Blob 
  | Tree Tree 
  | Commit Commit
  | AnnTag AnnTag

-- | Annotated tags are meant for releas
data AnnTag = MkAnnTag 
  { tagDate  :: Text 
  , tagName  :: Text
  , tagMsg   :: Text
  , tagEmail :: Text
  }
  
-- | large number of small files hurts performance 
-- we store a collection of objects in one file in 
-- the OS's file system
-- see `git repack`, `git count-objects` and
--     `git prune`
type PackFile = [Obj] 
        

-- ======================================================
-- Repository: object and ref store
-- ======================================================

-- | repository
data Repo = MkRepo 
  { _objs :: Map HashRef Obj
  -- ^ stored in .git/objects
  , _refs :: Map NameRef HashRef
  -- ^ stored in .git/refs
  }
$(makeLenses ''Repo)

store :: Obj -> Repo -> Repo
store o = objs %~ Map.insert (hashWith SHA1 o) o

load :: HashRef -> Repo -> Obj
load k repo = repo^.objs & (Map.! k)

deref :: Ptr a -> Repo -> a
deref ptr repo = case ptr of 
  BlobHash   k -> case loadHash k of { Blob   b -> b }
  TreeHash   k -> case loadHash k of { Tree   t -> t }
  CommitHash k -> case loadHash k of { Commit c -> c }
  BlobRef    r -> case loadRef  r of { Blob   b -> b }
  TreeRef    r -> case loadRef  r of { Tree   t -> t }
  CommitRef  r -> case loadRef  r of { Commit c -> c }
 where
  loadRef  r = loadHash (repo^.refs & (Map.! r))
  loadHash k = load k repo

-- ======================================================
-- STAGING AERA
-- ======================================================


```

# 2 Commands

1. `git commit --amend`

## 2.1 revision


## 2.2 Plumbing (low-level api)

1. `{bash}git hash-object -w some_dir/my_file`, computes object name (use `sha1`) from a file;
2. `{bash}git count-object`, count number of loose objects
3. view objects
  - `{bash}git ls-tree` show tree object
  - `{bash}git cat-file` show object pointed by tag
4. view index `{bash}git ls-files --staged`

## 2.3 Porcelain

### 2.3.1 inspect object
- `{bash}git show` view any object
### 2.3.2 tag/reference management 
- `{bash}git tag`
- `{bash}git describe` finds the most recent tag that is _reachable_ form a commit

### 2.3.3 branch management

conceptually a branch is a sub-tree rooted at branch head referenced by the _branch name_. Thus branch management cares about manipulating `.git/refs/heads` and `.git/HEAD`
- `{bash}git branch <branch-name> [<snapshot>]` creates a branch
- `{bash}git branch -d <branch-name>` deletes a `HEAD` upstream _fully merged_ branch;
- `{bash}git branch --set-upstream-to <remote-branch>` sets current branch's upstream branch to some remote's remote branch;

### 2.3.4 submodules

The `git submodule add <repo> <path>` command does a couple of things:
- It clones the submodule from `<repo>` to the given `<path>` under the current directory and by default checks out the master branch.
- It adds the submodule’s clone path to the [gitmodules(5)](https://git.github.io/htmldocs/gitmodules.html) file and adds this file to the index, ready to be committed.
- It adds the submodule’s current commit ID to the index, ready to be committed.

### 2.3.5 merge tool 

- `git mergetool` open merge tool to resolve conflicts;
- `git config --global merge.tool <tool-cmd>`

# 3 Tips 

1. use `namespace/someref` for naming branches and tags;
2. `{bash}git clean` to remove untracked files in working directory.
3. `-n` or `--dry-run` are used to preview dangerous git command actions, use it before irreversible commands(`{bash}git gc/purge/clean`)
4. use `{sh}git clone --shallow` to avoid clone the full history
5. (pickaxe) `{sh}git log -S <string>` show only the commits that changes the number of occurrences of the string;
6. `{sh}git log -L <code>` will show you the history of a function or line of code in your codebase.

# 4 Git Hooks & pre-commit

There are 2 groups of _hooks_:
1. _client-side hooks_ are triggered by operations such as committing and merging
2. _server-side hooks_ run on network operations such as receiving pushed commits.

>[!note] `hooks` subdirectory
> hooks are stored in `.git/hooks`.



# 5 FAQ

## 5.1 `git switch` and `git restore`

From [StackOverflow: the difference btw 'git switch' and 'git checkout'](https://stackoverflow.com/a/70454786/12816803)

`{bash}git checkout` has $2$ different semantics:

1. checking out a branch to work on advancing its history;
2. checking out paths out of the index and/or a tree-ish to work on advancing the current history.

on the other hand:

- `{bash}git switch` has only the 1st semantics, in other words, point `HEAD` to another snapshot, and make the _working tree_ and _index_ identical to the new `HEAD`;
- `{bash}git restore` only has the 2nd semantics, restore some _path in working tree and index_ from `HEAD`.

## 5.2 commit message

messages should start with a single line that’s no more than about 50 characters and that describes the change set concisely, followed by a blank line, followed by a more detailed explanation. The Git project requires that the more detailed explanation include your motivation for the change and contrast its implementation with previous behaviour — this is a good guideline to follow.

>[!example]
> ![[Pasted image 20241014121452.png]]

## 5.3 patch and interactive staging

`{bash}git add --patch` lets you select part of the modification to the index, it is equivalent to `git add --interactive` then select `p`.
  - `s` to split a hunk
  - `e` to manually edit a hunk

`{bash}git format-patch <some-remote-branch>` to create a patch for each commit from `some-remote-branch` to `HEAD`

# 6 Reference

- [concept glossary](https://git-scm.com/docs/gitglossary)

## 6.1 Books

- [[Git-Cheatsheet.pdf]]
- [Book: Pro Git](https://git-scm.com/book/en/v2)
- [manpage gitcore-tutorial(7)](https://git-scm.com/docs/gitcore-tutorial)
- [git concepts chapter of the user-manual](https://git.github.io/htmldocs/user-manual.html#git-concepts)

## 6.2 Videos
- [MIT Git Lecture](https://missing.csail.mit.edu/2020/version-control/)

## 6.3 Tips
- [Stack overflow: pretty git branch graphs](https://stackoverflow.com/questions/1057564/pretty-git-branch-graphs)
- [A note about git commit messages](https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html)
- [email + git = <3](https://git-send-email.io)
