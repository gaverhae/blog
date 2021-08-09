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
and tries to exploit the pattern recognition power of the human brain. The
`run` function takes a list of "encrypted" strings and starts an interactive
decryption loop. The optional second argument is a starting decryption
dictionary, presented as a 52-character string where even indices are the
letter to replace and the following character is its replacement. For example,
to start decrypting the first encrypted page from scratch:

```clojure
t.core=> (run ["Mnbrvcnp!"
               "Wiup! Rdd is fm, wiup! F tru'm epdfpjp fm'v rdd wiup!"
               "F wim prmpu eh r ynrwiu sin mqrm snprzfu' mnbrvcnp!"
               "F wim jiafmpy ck! Eh r yfvwcvlfuw rtfy-enprmqfuw ynrwiu! Ruy uix fm'v WIUP!"
               "Uim MQP diim. AH diim. Afup!"
               "Xrfm, xqh yi F vicuy scuuh? Qpddi? Qpddi?"
               "Xqrm? Ui! Yiu'm hic yrnp!"
               "Vuprz rmmrtz eiim mi mqp srtp!"
               "Yrnu vmnrfwqm."])
```

To start with what I believe to be the correct key, run:

```clojure
t.core=> (run ["Mnbrvcnp!"
               "Wiup! Rdd is fm, wiup! F tru'm epdfpjp fm'v rdd wiup!"
               "F wim prmpu eh r ynrwiu sin mqrm snprzfu' mnbrvcnp!"
               "F wim jiafmpy ck! Eh r yfvwcvlfuw rtfy-enprmqfuw ynrwiu! Ruy uix fm'v WIUP!"
               "Uim MQP diim. AH diim. Afup!"
               "Xrfm, xqh yi F vicuy scuuh? Qpddi? Qpddi?"
               "Xqrm? Ui! Yiu'm hic yrnp!"
               "Vuprz rmmrtz eiim mi mqp srtp!"
               "Yrnu vmnrfwqm."]
              "ambecudlebfig?hyiojvkpltmtnro?peqhrasftcunvswgxwydzk")
```

If you only have a partial decryption key to start with, replace any unknown
letter with `?`.

Once the interactive loop has started, type `?` on a line by itself for
instructions.

This program uses a disctionary; it has been generated from
[SCOWL](http://app.aspell.net/create).
