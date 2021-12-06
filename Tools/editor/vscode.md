# Debug

[GDB - Basic Setup — Debugging documentation (unsw.edu.au)](https://www.cse.unsw.edu.au/~learn/debugging/modules/gdb_setup/)

```json

{
    // Use IntelliSense to learn about possible attributes.
    // Hover to view descriptions of existing attributes.
    // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Python: Current File",
            "type": "python",
            "request": "launch",
            "program": "${file}",
            "console": "integratedTerminal"
        },
        {
            "name": "afl-fuzz launch",
            "type": "cppdbg",
            "request": "launch",
			// target program
            "program": "${workspaceFolder}/afl-fuzz",
			// args passed to target program
            "args": [
                "-t", "10000",
                "-i", "$PNG_INPUT",
                "-o", "$AFL_OUTPUT",
                "$AFL_TARGET",
                "@@"
            ],
			// if stop at the `main` function
            "stopAtEntry": false,
            "cwd": "${fileDirname}",
			// env variables, use ${env:NAME}
            "environment": [
                {
                    "name": "PNG_INPUT",
                    "value": "${env:AFL}/testcases/images/png"
                },
                {
                    "name": "AFL_OUTPUT",
                    "value": "${workspaceFolder}/afl-output"
                },
                {
                    "name": "AFL_TARGET",
                    "value": "/home/zys/fuzz/libpng-afl/pngimage"
                }
            ],
            "externalConsole": false,
            "MIMode": "gdb",   // use gdb or lldb
            "setupCommands": [
                {
                    "description": "Enable pretty-printing for gdb",
                    "text": "-enable-pretty-printing",
                    "ignoreFailures": true
                }
            ]
        }
    ]
}

```