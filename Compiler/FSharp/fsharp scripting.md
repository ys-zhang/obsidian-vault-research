# Scripting

  

- We use `#load "path/to/script.fsx"` to make it available and then open `NameOfFileWithoutExtension` to import it. So each script file is then treated as a module.
- You can reference DLLs using `#r "path/to/file.dll"`