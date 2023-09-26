# terminfo

These is a _terminfo_ database containing the capability and escape control bytes used to support:
- cursor movement
- italics, bold, standout, etc
- ...
The env var `TERMINFO` specifies the path of the database used,
by convention it can be `$HOME/.terminfo`, if it is unset then `/usr/share/terminfo` is used

command `infocmp` can be used to print out the data in the database.

the database uses _term name_ to specify information for different kind of terminal emulators.

The env var `TERM` is set to specify the emulator variant.

on mac, command `tic` can be used to modify the database.

a modification is sth like:
```
screen-256color|GNU Screen with 256 colors,
     sitm=\E[3m, ritm=\E[23m,
     smso=\E[7m, rmso=\E[27m,
     use=screen,

```
sets 
1. _italics_: with `sitm` and `ritm` controls start and end of _italic_ context
2. _standout_: with `smso` and `rmso` to set mark standout and remove mark standout. Usually _standout_ is implemented by _reverse video_ with reverses the fg and bg color settings


To check that the terminal does the right thing:

-   with fish:
```fish
echo (tput sitm)'italics' (tput ritm) (tput smso)'standout'(tput rmso)
```    
-   with bash or zsh:
```bash
echo `tput sitm`italics`tput ritm` `tput smso`standout`tput rmso`
```

