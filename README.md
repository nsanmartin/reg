# reg

This program reads lines from stdin and copy each in a "register".  It also
copies to clipboard the input received. If no stdin is provided, an arbitrary
set of registers, requested by some QUERIES are displayed in stdout and also
copied to clipboard. A query is a string that starts with register identifiers
and may optionaly have a `.` (dot) followed by word-indices. The register
identifiers are : `0123456789abcdefghijklmnopqrstuvwxyz`. So for example: `0ac`
refers to the three lines cointained at registers 0, a and c while `017.02`
means one line containing the first and third words from registers 0, 1 and 7.
That is, when word-indices are used the response is put in one line while when
they're absent it would have one line per register in the query.


Say you want to cherry pick certain commits:

```
$ git log --oneline | reg
"0 6426f19 Use the dot to separate regs from indexes
"1 5a84686 Add word indexing
"2 08c5539 Fix get reg case
"3 389b3ff Show invalid regs
"4 2bfc6c1 Limit regfile size
"5 0060462 Remove trailing newlines when copying to clipboard
"6 415a146 Add setClipboard to stdin and regs operations
"7 d816dbd Use regfile as store
"8 b1e1272 Reading and writing from regfile
"9 7ba295c Add user interface
"a 15d23f2 First commit
```

After running that then

```
$ git cherry-pick `reg a975.0`
```

Will be 
```
$ git cherry-pick r15d23f2 7ba295c d816dbd 0060462

```

Or you may also paste from the clipboard the string with the commit hashs.

But the running `reg a.0 9.0 7.0 5.0` displays:
```
r15d23f2
7ba295c 
d816dbd 
0060462
```

## Install

First, install [the haskell stack tool](https://docs.haskellstack.org/en/stable/).

Then:
```
git clone https://github.com/nsanmartin/reg.git
cd reg
stack install reg
```

You also have to create the reg file, and may set an alias for editing regs:

```
mkdir -p $HOME/.reg && touch $HOME/.reg/regfile
alias regf='vi $HOME/.reg/regfile'
```


