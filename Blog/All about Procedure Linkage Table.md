#linker 

copy from [All about Procedure Linkage Table | MaskRay](https://maskray.me/blog/2021-09-19-all-about-procedure-linkage-table)

see also [[ELF (Executable and Linkable Format)]]

# Branch target

Many architectures encode a branch/jump/call instruction with PC-relative addressing, i.e. the distance to the target is encoded in the instruction. 

In an executable or shared object (called a component in ELF), if the target is bound to the same component, the instruction has a fixed encoding at link time; otherwise the target is unknown at link time and there are two choices:
- text relocation
- indirection

