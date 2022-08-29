[Getting Started · tmux/tmux Wiki (github.com)](https://github.com/tmux/tmux/wiki/Getting-Started)


# tmux Model and Concepts

-   Programs run in terminals in **panes**, which each belong to one **window**.
-   Each window has a name and one active pane.
-   **Windows** are linked to one or more **sessions**.
-   Each **session** has a list of **windows**, each with an index.
-   One of the windows in a session is the **current window**.
-   **Sessions** are attached to zero or more **clients**.
-   Each **client** is attached to one **session**.

![[Pasted image 20220722162119.png]]

# Cheat sheet

## Prefix Keys

press the _prefix keys_ twice to send it to the current running program in the active pane.


## Key bindings

| Key binding    | Meaning            |
| -------------- | ------------------ |
| `C-b ?`        | List key bindings  |
| `C-b / [keys]` | help on `keys`     |
| `C-b :`        | enter command mode |
| `C-b d`        | detach from session |
|                |                    |


## Commands

### Session
`ls`, `list-session`: list sessions
`attach`: attach a session

### Window

- `C-b c`: runs the `new-window` command
- `new-window`, `neww`: command creates a new window 
    - `-n [name]` flag, set window's name to `name`
- `split-window` command splits the window into 


