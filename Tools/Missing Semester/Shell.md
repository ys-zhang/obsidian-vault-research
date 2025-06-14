#missing-semester

# 1 Bash



## 1.1 Basic Concepts

1. _simple command_: `a b c d` The first word generally specifies a command to be executed, with the rest of the words being that command’s arguments.
2. A _pipeline_ is a sequence of one or more commands separated by one of the _control operators_ `|` or `|&`.
```bash
[time [-p]] [!] command1 [ | or |& command2 ] …
```
  - `time` for collecting time statistics;
  - `!` negates the commands exit code `$?`
  - `|&` is short for `2>&1 |` which merges the output and error stream.
3. A _list of commands_ is a sequence of one or more pipelines separated by one of the operators `;`, `&`, `&&`, or `||`, and optionally terminated by one of `;`, `&`, or a `newline`. 
  - `;` is sequence operator;
  - `&` forces the the previous command to be **execute in background**;
  - `&&` and `||` are logical connectives.
4. `{sh}coproc [NAME] { command; }` runs the command asynchronously, the `NAME` argument is the name of the variable that 
  - `NAME[0]` is the file descriptor connects to commands output stream;
  - `NAME[1]` is the file descriptor connects to commands input stream;

## 1.2 Job control

see also [[control terminal, job control and session]]

>[!tip]
> - the `{bash}kill` command can send any signal to a process
> - `pkill` is much handier than `kill`
> - `{bash}nohup <CMD>` command is a command wrapper that makes inner command immune to `SIGHUP`
> - The `{bash}jobs` command lists the unfinished jobs associated with the current terminal session;
>   - use `%n` to refer to the $n$th^[in list from `jobs` and start from 1] job's PID.
> - `{bash}pgrep` searches all processes using the process table;
> - `$!` refers to the last job's PID;
> - `wait <PID>` waits for the job to to finish before proceeding.   



>[!note] kill processes
>
> | signal  |   keymap/command   |       semantics       |
> | :-----: | :----------------: | :-------------------: |
> | SIGINT  |       Ctrl-C       |       interrupt       |
> | SIGQUIT |       Ctrl-\       |     quit program      |
> | SIGTERM | `kill -TERM <PID>` | software termination  |
> | SIGHUP  |                    | the terminal hangs up |


>[!note] pausing & backgrounding processes
> 
> |     signal      |    keymap/command     |      semantics      |
> | :-------------: | :-------------------: | :-----------------: |
> | SIGSTOP/SIGTSTP |        Ctrl-Z         | stop/pause program  |
> |     SIGCONT     | `fg <PID>`/`bg <PID>` | continue after stop |

see also [[signal]].


## 1.3 Pitfalls

1. permission deny
  - `{bash}sudo ls | needs-root` is interpreted as run `sudo` with argument `ls`, not run `sudo` with argument `{bash}ls | needs-root`. If you want to run the whole command in root mode you can either
    - use some thing like `exec`, `bash` ...
    - use `tee`
    - use `sudo -i` to inter an interactive mode
1. side effects
  - the current environment will not get polluted by running/executing a shell script as a command; however, 
  - `{sh}source` a script can.
2. split after expansion/substitution
  - `{bash} name='Mister Noodle' ; mkdir $name` will create $2$ directories;
  - in fish, the above line will only create $1$ directory.
3. closing terminal will send `SIGHUP` to all running child processes (no matter background or foreground) of the terminal, use `{bash}nohup` command to keep background processes running.

>[!important] function & script
> - _function_ is usually used by `source` file in which it is defined, and side effects of the function can change the shell environment.
> - _script_ is usually used as commands, and thus more "pure".

## 1.4 Tips

1. the prompt `#` indicating current user is _root_.
2. `{bash}cd -` changes to last working directory.
3. the `{bash}tee [-ai] [file ...]` command is extremely handy when debugging and **duplicate** stream redirection (and piping). It pass input to output and prints the input regardless of where output is redirect to.
4. `{sh}grep` will tricker an _error_ if no match is found, which is useful with `$?`.
5. `{sh}/dev/null` is a perfect file to dump useless data.
6. with the _shebang_^[for example `#!/usr/bin/python`], we can use `env PROG` to run the program found under the environment search path.
7. (debug) use `shellcheck` to lint shell scripts.
8. find
    - `find`(`fd`) can have a `-exec`(`--exec` or `-x`) argument that execute a command on every matching file.
    - `locate` uses an index database to faster find. In _MacOS_ the command is `mdfind`^[find using central metadata store.]
9. history
  - use `{sh}history` to fast access previous commands
  - `Ctrl-R` opens a search panel for history

### 1.4.1 fzf

fzf is a tool for interactive fuzzy finding.

## 1.5 Cheatsheets

### 1.5.1 Special Parameter

```bash
#!/bin/bash  
echo "Number of argument passed: $#"  
echo "Script name is $0"  
echo "The 2nd argument passed is: $2"  
echo "Arguments passed to script are: $*"  
echo "Exit status of last command that executed:$?" 
echo "Last argument provide to previous command:$_"  
echo "PID of current shell is: $$"  
echo "Flags are set in the shell: $-"
```

>[!note] `$@` and `$*` 
> `$*` is a single string, whereas `$@` is an actual array.

### 1.5.2 Operators 
1. (logic) `&&`, `||` are lazy;
2. (sequence), `c1 , c2` runs 2 commands in sequence

### 1.5.3 Shell expansion

>[!note] 
>shell expansion is similar to macro, which can be seen as a specific pass of the bash interpreter, which _performed on the command line after it has been split into tokens_.

Expansion order:
1. _Brace expansion_ is a mechanism by which arbitrary strings may be generated. `{bash}echo a{d,c,b}e`, see also [[#1.4.4 Glob|glob]];
2. _tilde expansion_ is basically `~` => `$HOME`;  
3. _shall parameter expansion_ expands variables to its value;
4. _command substitution_, `$( CMD )`^[or quote with backtick]
6. _process substitution_, `<( CMD )` will execute `CMD` and place the output in a temporary file and substitute the `<()` with that file’s name.^[This is useful when commands expect values to be passed by _file_ instead of by _STDIN_]

### 1.5.4 Glob

>[!def] glob
>In Bash, the term "glob" refers to **the process of pattern matching or globbing**, which is a way to match filenames and paths using wildcard characters. For detail see [pattern-matching in bash](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html)


>[!tldr]
>Glob patterns are like simplified regular expressions that shells use. 
>- An asterisk (`*`) matches zero or more characters; 
>- `[abc]` matches any character inside the brackets (in this case a, b, or c);
>- a question mark (`?`) matches a single character; and 
>- brackets enclosing characters separated by a hyphen (`[0-9]`) matches any character between them (in this case 0 through 9). 
>- You can also use two asterisks to match nested directories; `a/**/z` would match a/z, a/b/z, a/b/c/z, and so on.

# 2 Fish

1. No _bangbang_
  - use `Alt`+`S`for `{bash}sudo !!`
  - `Alt`+`ArrowUp` recalls arguments from prev command.
  - `Alt+.` to bring up history.
2. Special parameters:

|         bash         |       fish        |
| :------------------: | :---------------: |
| `$*`, `$@`, `$1` ... |      `$argv`      |
|         `$0`         | `status filename` |
|         `$?`         |     `$status`     |
|         `$$`         |    `$fish_pid`    |
|         `$#`         |   `count $argv`   |
|         `$!`         |    `$last_pid`    |
|         `$-`         | `status` function |
3. _process substitution_^[`<()` in bash] in fish is a command called `psub` 
```fish
diff (sort a.txt | psub) (sort b.txt | psub)
```
4. history
  - `dirh` is a fish command prints the current directory history
  - `Ctrl-R` opens a search panel for history
5. interactive use 
  - `ArrowRight`/`Ctrl-F` accepts autosuggestion;
  - `Alt+ArrowRight`/`Alt-F` accepts the first suggested word;
  - `Tab` triggers autosuggestion;
  - `Ctrl-R` opens a search panel for history
6. redirect streams
  - same for `<`, `>` and `2>` in bash;
  - `&>` redirects both stdout and stderr.

# 3 Remote Development (SSH)

- use `ssh-keygen` to create pub/private keys;
- `ssh-agent` for SSO (single sign-on):
  - `ssh-add` adds identifies to the agent, `ssh-add -l` lists all the identities;
  - `ssh-copy-id` use locally available keys to authorise logins on a remote machine

>[!def] agent forwarding
> a mechanism whereby an [SSH client](https://www.ssh.com/ssh/client) allows an [SSH server](https://www.ssh.com/ssh/server) to use the local `ssh-agent` _on the server_ the user logs into, as if it was local there.
> SSH agent forwarding 可以讓本地的 SSH Key 在遠端 Server 上進行轉送，也就是當你需要在選端 Server 上使用 SSH Key 時，就不需要將你的 key pair 手動複製到 server 上，是個暨方便又安全的作法。
>
> 舉例來說，首先 SSH 登入進 Server1，接著在 Server1 上登入 Server2 時，就會自動使用你本地的 SSH Key：
>
> `Local ---(SSH)---> Server1 ---(SSH)---> Server2`

## 3.1 Copy from/to remote server

- `ssh+tee`, the simplest is to use `ssh` command execution and STDIN input by doing `cat localfile | ssh remote_server tee serverfile`. Recall that [`tee`](https://www.man7.org/linux/man-pages/man1/tee.1.html) writes the output from STDIN into a file.
- [`scp`](https://www.man7.org/linux/man-pages/man1/scp.1.html) when copying large amounts of files/directories, the secure copy `scp`command is more convenient since it can easily recurse over paths. The syntax is `scp path/to/local_file remote_host:path/to/remote_file`
- [`rsync`](https://www.man7.org/linux/man-pages/man1/rsync.1.html) improves upon `scp` by detecting identical files in local and remote, and preventing copying them again. It also provides more fine grained control over symlinks, permissions and has extra features like the `--partial` flag that can resume from a previously interrupted copy. `rsync`has a similar syntax to `scp`.
- `sshfs` can mount a remote folder

## 3.2 Port forwarding

_Port forwarding_ comes in two flavours: 
1. Local Port Forwarding 
2. Remote Port Forwarding

[great post about SSH port forwarding](https://unix.stackexchange.com/a/115906)

