[Getting Started · tmux/tmux Wiki (github.com)](https://github.com/tmux/tmux/wiki/Getting-Started)

# 1 Model and Concepts

```
+----+   +----+
|Proc|-->|Pane|----+
+----+   +----+    |       +------+
                   |-----> |Window|---+
+----+   +----+    |       +------+   |
|Proc|-->|Pane|----+                  |    +-------+
+----+   +----+                       |--->|Session| 
                                      |    +-------+
                           +------+   |
                    -----> |Window|---+
                           +------+
```

-   Programs run in terminals in **panes**, which each belong to one **window**.
-   Each window has a name and one _active pane_.
-   **Windows** are linked to one or more **sessions**.
-   Each **session** has a list of **windows**, each with an index.
-   One of the windows in a session is the **current window**.
-   **Sessions** are attached to zero or more **clients**.
-   Each **client** is attached to one **session**.

Status Line:
![[Pasted image 20220722162119.png]]


# 2 Cheat sheet

## 2.1 Prefix Keys

press the _prefix keys_ twice to send it to the current running program in the active pane.


## 2.2 Key bindings

| Key binding    | Meaning            |
| -------------- | ------------------ |
| `C-b ?`        | List key bindings  |
| `C-b / [keys]` | help on `keys`     |
| `C-b :`        | enter command mode |
| `C-b d`        | detach from session |
|                |                    |

memo trick
1. _Session_ starts with `s`, keys are capitalised if there is a conflict
    - `C-b s`: new **session**
    - `C-b $` rename a session, `$` symbol is much like an `s`
    - `C-b L`: **last** session
    - `C-b (`, `C-b )` cycling sessions
2. Window
    - `C-b c` **create** a new window
    - `C-b ,` name a window
    - `C-b l` **last** window
    - `C-b p`, `C-b n` cycling windows, (prev/next)
    - `C-b &` kill/**end** a window, `end` pronounces like `and`
    - `C-b f` **find** a window

## 2.3 Commands

### 2.3.1 Session
`ls`, `list-session`: list sessions
`attach`: attach a session

### 2.3.2 Window

- `C-b c`: runs the `new-window` command
- `new-window`, `neww`: command creates a new window 
    - `-n [name]` flag, set window's name to `name`
- `split-window` command splits the window into 


# 3 Config File

path: `~/.tmux.config`.




