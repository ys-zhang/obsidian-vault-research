

1. `{bash}sed` for stream editor, which lets you change content of streams.
  - substitution command: `s/REGEX/SUBSTITUTION/` which substitute parts of any item in the stream that matches the regex.
  - use a different separator other than '/', for instance `#` is better than `/` when dealing paths;
  - use `-E` option to use full version of regular expression;
2. `sort` & `unique`
  - `uniq -c` will add a count column;
3. `awk` is a column based stream editor;
4. `paste` glues multiple lines into one single line;
5. `bc` is a calculator;
6. `R --slave -e 'x <- scan(file="stdin", quiet=TRUE); summary(x)'`
7. `xargs` consumes multiple lines of input and turn them into arguments;
