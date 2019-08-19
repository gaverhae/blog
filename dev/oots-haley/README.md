# Haley Unscrambler

In the fabulous [Order of the Stick](http://www.giantitp.com/comics/oots.html)
comics, at some point, Haley loses her ability to talk and instead produces
gibberish when trying. It turns out that she is actually talking using a fairly
simple encryption algorithm based on a substitution table. The main trick is
that the table is different on each page.

This repository contains code (and solutions) for decrypting those texts. This
is not using some advanced statistical analysis; this is a helper tool meant to
support a human in the process of decrypting, not a standalone tool. I believe
the return on investment is much higher when designing tools that combine human
intelligence with good supporting tools than when trying to build all around
pure-machine solutions (aka "AI").

## How does it work?

Because it is designed to help out a human, this is an interactive program that
takes out the tedious parts of keeping track of the mapping identified so far,
and tries to exploit the pattern recognition power of the human brain. Here are
the supported commands:

* `:f grkkO` generates a list of words that match the given pattern and show
  the first five.
* `.` moves through the above list of words.
* `:r` resets the current key to an ampty one.
* `:q` exits current function loop.
* `:k` resets the current key to the suggested (hardcoded) one.
* `:g 247` jumps to comic page 247.
* `grkkt hello` updates the current key to map `g` to `h`, `r` to `e`, `k` to
  `l` and `t` to `o`.
* `ghrekl` updates the current key to map `g` to `h`, `r` to `e`, and `k` to
  `l`.

The dictionary has been generated from [SCOWL](http://app.aspell.net/create). A
pattern in this case is a mix of possibly repeated but unknown letters and
known letters; in the given example above, `:f grkkO` would supply, in random
order, all of the words from the dictionary that satisfy the following
conditions:

* The word ends with the letter `o` (because it is provided asuppercase).
* The word has exactly 5 letters (because the given pattern has five letters).
* The word is composed of four different letters, say 0, 1, 2 and 3, where 3 is
  o and the letters are arranged as 0122o. Example: `hello`.
* None of the unknown letters map to a letter that is already mapped to in the
  key.
