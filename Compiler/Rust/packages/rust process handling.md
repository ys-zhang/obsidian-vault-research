[std::process - Rust (rust-lang.org)](https://doc.rust-lang.org/std/process/index.html)

使用 `Command` 结构 设置 spawn 子进程
```rust
use std::process::{Command, Stdio};

Command::new("bash")         // target program 
        // args
        .arg("./script.sh")  // one single arg
        .args(["-l", "-a"])  // multiple args
        // env
        .env("VAR", "value") // set env variable
        .env_remove("VAR_TO_RMV")
        .curr_dir("./path/to/workdir") // set PWD
        // std io stream
        .stdin(Stdio::null())  // reset stdin
        .stdout(Stdio::null())  // reset stdout
        .stderr(Stdio::null())  // reset stdout
        // Executes as a child process, 
        //   returning a handle to it.
        .spawn()  // type Result<Child>
        .expect("ls command failed to start");

// Executes the command as a child process,
//   waiting for it to finish and collecting 
//   all of its output.
Command::new("ls")
        .output() // type: Result<Output>
```

# Example

```rust
// From LibAFL
// Configure the target. setlimit, setsid, pipe_stdin,
//   I borrowed the code from Angora fuzzer
pub trait ConfigTarget {
    fn setsid(&mut self) -> &mut Self;
    fn setlimit(&mut self, memlimit: u64) -> &mut Self;
    fn setstdin(&mut self, fd: RawFd, use_stdin: bool) -> &mut Self;
    fn setpipe(
        &mut self,
        st_read: RawFd,
        st_write: RawFd,
        ctl_read: RawFd,
        ctl_write: RawFd,
    ) -> &mut Self;
}

impl ConfigTarget for Command {
    fn setsid(&mut self) -> &mut Self {
        let func = move || {
            unsafe {
                libc::setsid();  // create a new session
            };
            Ok(())
        };
        unsafe { self.pre_exec(func) }  // execute func before fork done
    }

    fn setpipe(
        &mut self,
        st_read: RawFd,
        st_write: RawFd,
        ctl_read: RawFd,
        ctl_write: RawFd,
    ) -> &mut Self {
        let func = move || {
            match dup2(ctl_read, FORKSRV_FD) {
                Ok(_) => (),
                Err(_) => {
                    return Err(io::Error::last_os_error());
                }
            }

            match dup2(st_write, FORKSRV_FD + 1) {
                Ok(_) => (),
                Err(_) => {
                    return Err(io::Error::last_os_error());
                }
            }
            unsafe {
                libc::close(st_read);
                libc::close(st_write);
                libc::close(ctl_read);
                libc::close(ctl_write);
            }
            Ok(())
        };
        unsafe { self.pre_exec(func) }
    }

    fn setstdin(&mut self, fd: RawFd, use_stdin: bool) -> &mut Self {
        if use_stdin {
            let func = move || {
                match dup2(fd, libc::STDIN_FILENO) {
                    Ok(_) => (),
                    Err(_) => {
                        return Err(io::Error::last_os_error());
                    }
                }
                Ok(())
            };
            unsafe { self.pre_exec(func) }
        } else {
            self
        }
    }

    fn setlimit(&mut self, memlimit: u64) -> &mut Self {
        if memlimit == 0 {
            return self;
        }
        let func = move || {
            let memlimit: libc::rlim_t = (memlimit as libc::rlim_t) << 20;
            let r = libc::rlimit {
                rlim_cur: memlimit,
                rlim_max: memlimit,
            };
            let r0 = libc::rlimit {
                rlim_cur: 0,
                rlim_max: 0,
            };

            #[cfg(target_os = "openbsd")]
            let mut ret = unsafe { libc::setrlimit(libc::RLIMIT_RSS, &r) };
            #[cfg(not(target_os = "openbsd"))]
            let mut ret = unsafe { libc::setrlimit(libc::RLIMIT_AS, &r) };
            if ret < 0 {
                return Err(io::Error::last_os_error());
            }
            ret = unsafe { libc::setrlimit(libc::RLIMIT_CORE, &r0) };
            if ret < 0 {
                return Err(io::Error::last_os_error());
            }
            Ok(())
        };
        unsafe { self.pre_exec(func) }
    }
}
```