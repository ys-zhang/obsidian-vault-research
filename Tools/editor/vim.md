- [CS 110L: Safety in Systems Programming (reberhardt.com)](https://reberhardt.com/cs110l/spring-2021/handouts/tools-tips/)
- [Vim Cheat Sheet (rtorr.com)](https://vim.rtorr.com/)
- [Vim FaQ](https://vimhelp.org/vim_faq.txt.html)
- [Learn Neovim The Practical Way](https://alpha2phi.medium.com/learn-neovim-the-practical-way-8818fcf4830f)

# Basic

- `:help vim-modes`: help about vim modes 
- `:view {file}` open in read-only mode


# Neovim Commands

| full                         | short  | description                         |
| ---------------------------- | ------ | ----------------------------------- |
| `:{count}mes[sages] [clear]` | `:mes` | show/clear message history          |
| `:se[t] {option}?`           | `:se`  | options, see `:help option-summary` |
| `:view {file}`               |        | open file in read-only mode         |
| `:b[uffer] {name}`           | `:b`   | switch btw buffers                  | 


# Tag 

`:tselect {pattern}`, select tags in current buffer

# Help

- `<Ctrl-]>`  jump in to _tag_.
- `<Ctrl-T>` jump back to previous position
- `:help {subject}` get help of the subject
- `:help index`:  a list of all available _subjects_
- type `<Ctrl-D>` after `:help` to get a list of all the help keywords containing the supplied pattern.
    - `:help init<C-D>`
    - `:help str*<C-D>`
    - `:help '*indent<C-D>`
- `:helpgrep` 


# Vim Model

```
                                     +------+
                                 +---|Window|
                      +---+      |   +------+
                 +----|Tab|------+
+--------+       |    +---+      |   +------+      +-------------+
|Vim Proc|-------+               +---|Window|------|active buffer|
+--------+       |    +---+          +------+      +-------------+
                 +----|Tab|
                      +---+
```

A **status line** is used to separate windows.

# LSP Model

```
                      +------+      +------+
                +-----|Client|------|Server|
                |     +------+      +------+
+--------+      |
| Buffer |------+
+--------+      |
                |     +------+      +------+
                +-----|Client|------|Server|
                      +------+      +------+
```

Buffer and Clients are implemented in Neovim, but Server are standalone apps lives outside.

see 
- `:h lsp`
- `:h lspconfig`
