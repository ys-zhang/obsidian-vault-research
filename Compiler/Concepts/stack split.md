[SplitStacks - GCC Wiki (gnu.org)](https://gcc.gnu.org/wiki/SplitStacks)

The goal of split stacks is to permit a discontiguous stack which is grown automatically as needed. This means that you can run multiple threads, each starting with a small stack, and have the stack grow and shrink as required by the program.