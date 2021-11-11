![[Pasted image 20210911123430.png]]
# Preprocessor
[GCC Preprocessor Options](https://gcc.gnu.org/onlinedocs/gcc-11.2.0/gcc/Preprocessor-Options.html#Preprocessor-Options)

- **-E** only do pre-processing
- **-D** ***name*** define a macro

	The contents of definition are tokenized and processed as if they appeared during *translation phase three* in a `#define` directive. In particular, the definition is truncated by embedded newline characters.

	If you are invoking the preprocessor from a shell or shell-like program you may need to use the shell’s quoting syntax to protect characters such as spaces that have a meaning in the shell syntax.

	If you wish to define a function-like macro on the command line, write its argument list with surrounding parentheses before the equals sign (if any). Parentheses are meaningful to most shells, so you should quote the option. With `sh` and `csh`, -D'name(args…)=definition' works.

	-D and -U options are processed in the order they are given on the command line. All -imacros file and -include file options are processed after all -D and -U options.