#fuzzing 

# Operation Mode

## Injected



# Architecture

- **core** (_injector_ and all of the _glue_)
- **language bindings** (C, Python, Node, .NET, etc.)
- **runtime bridges** for ObjC & Java
- **cli tools**

![](https://lh5.googleusercontent.com/O2gIr8tO4C2CbqIM6z0lnLQKyDwenUals0YQKAOd_jExui4nLchoqVwegbsRAnG9pIscua9tuM3ql5fznrSWPbYBlUSuMztDhFgdMT830MDjbB0-opG930OooHl-R3zXe1jiMmzt5xTXjUeUCTjuFGfxOMioLy46AdcTYbKzVfxkMFWPluiAO_bTLVvRSLM)

>[!NOTE] Arch of Frida
>A Javascript runtime is injected to the target process.
>
>python host communicate with the injected javascript runtime through `send/recv`(js side) and `on/post`(_python side_) or _RPC_.


# Blogs & Tutorials

[Frida A world-class dynamic instrumentation framework | Inject JavaScript to explore native apps on Windows, MacOS, GNU/Linux, iOS, Android, and QNX](https://frida.re/)

> It’s a dynamic code instrumentation toolkit. It lets you inject snippets of JavaScript or your own library into native apps on Windows, MacOS, GNU/Linux, iOS, Android, and QNX.


<iframe width="560" height="315" src="https://www.youtube.com/embed/CLpW1tZCblo" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

[All about Procedure Linkage Table | MaskRay](https://maskray.me/blog/2021-09-19-all-about-procedure-linkage-table)


- [radare2](https://github.com/radareorg)
- [binary ninja](https://binary.ninja)
