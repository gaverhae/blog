{:title "Bash functions are better than I thought"
 :layout :post
 :tags ["bash"]}

I've been using Bash functions quite liberally over the past couple years, but
last week I discovered that they can be much better than I thought. In this
post, I'll explain how I thought they worked, why I was wrong, and how that
makes them better.

## Function syntax

I've always seen the syntax of Bash functions presented as:

```bash
function_name() {
  # code
}
```

While that does indeed produce a function, that's not the whole story. The
actual syntax is (from [the Bash manual][man]):

```bash
function_name() compound-command [ redirections ]
```

where `{ code; }` is just one of the possible options for a command list. Other
options include looping and conditional constructs (which in this context
include `(())` and `[[]]`), as well as _subshells_.

If the subshell syntax is used when defining a function, running the function
will result, as one should expect, in the code of the function running _in a
subshell_.

So what? Why am I excited about that? First, let's take a look at a few of the
drawbacks of Bash functions (specifically, the ones defined using `{}`).

## Function local state

Bash functions can have "local" variables declared with the keyword `local`.
Otherwise, variables defined within a function body are global variables,
because the function body (when using `{}`) executes in the same Bash
environment as the rest of the file.

I've often seen the recommendation to use `local` liberally with Bash
functions. That makes sense, as clobbering the global environment would be bad.
But it's annoying to have to remember to do it, and it's a bit tedious (and
error-prone) when refactoring code.

These variables are also somewhat less local than one would expect: they are
actually _dynamically scoped_, as in, functions down the call stack can see
them. Here's an example of what that means:

```plaintext
$ cat dynamic.sh
#/usr/bin/env bash

set -euo pipefail

func1() {
    local a
    a=1
    func3
}

func2() {
    local a
    a=2
    func3
}

func3() {
    echo $a
}

func1
func2
$ ./dynamic.sh
1
2
$
```

Chances are you did not know that. I didn't until recently. Overall, since the
first (arguably accidental) introduction of the idea in the original Lisp in
1958, the programming world seems to have largely agreed that _dynamic scope is
**bad**_, and that we should use lexical scope instead.

Now, perhaps you're thinking "if it's a bad idea, I can just not use it", and
that's the right attitude. However, there is a chance it may still happen to
you accidentally through some typo or refactoring. It's just one more trap to
look out for.

## Nested functions

Nested functions are a great tool to organize code into little, well-scoped
chunks. But with Bash functions (`{}`) living in their parent's scope, they may
not work as one would expect. See:

```plaintext
$ cat nested.sh
#!/usr/bin/env bash

set -euo pipefail

func1() {
    func3() {
        echo "within func1"
    }
    func3
}

func2() {
    func3() {
        echo "within func2"
    }
    func3
}

func1
func3
func2
func3
$ ./nested.sh
within func1
within func1
within func2
within func2
$
```

Yes, calling either `func1` or `func2` above (re)defines a _global_ `func3`.
Perhaps you're now thinking:

> Yes, that makes sense, because by default variables are global. But you just
> need to declare `func3` as local.

Well, here you go:

```plaintext
$ cat nested.sh
#!/usr/bin/env bash

set -euo pipefail

func1() {
    local func3
    func3() {
        echo "within func1"
    }
    func3
}

func2() {
    local func3
    func3() {
        echo "within func2"
    }
    func3
}

func1
func3
func2
func3
$ ./nested.sh
within func1
within func1
within func2
within func2
$
```

So that doesn't work. Of course, you can still "just not call" `func3` outside
the scope where you _intend_ for it to be defined, and make sure that _at that
point_ it has the right definition, and that should work out. Right?

## Cleanup

One of my favourite Bash features I discovered recently (i.e. in the last
couple years) is the ability to trap `EXIT`. I had been aware of signals for a
while, but I'd never really had a good use-case for trapping the usual ones
(TERM & KILL being the ones that come to mind most readily). But since I
discovered that you can trap the "synthetic" `EXIT` signal, I've been using it
quite a bit for clean-up. Create a temp file? Delete it at the end:

```bash
tmp=$(mktemp)
trap "rm -f $tmp" EXIT
```

Not only do you not need to remember to delete that file twenty lines below,
you're also guaranteed that you will (at least try to) delete that file even if
the program exits with an error, or gets killed.

Why am I mentioning this in the context of functions? Well, because it simply
doesn't work for them: returning from a function does not trigger the `EXIT`
signal. So if you want to write composable functions in Bash that have some
side-effect-y clean-up you want to encapsulate, you have two options:

- Give up on clean-up if you crash. That's the easy way out, but in many cases
  it's decidedly _not great_. You shouldn't have to make your software more
  brittle just because you want to introduce some syntactic abstractions.
- Override `EXIT`, and try to do it properly. Something like:

```bash
my_fun() {
    local restore_trap tmp
    restore_trap=$(trap -p EXIT)
    tmp=$(mktemp)
    cleanup="rm -f $tmp"
    trap "$cleanup; $(echo $restore_trap | sed "s/trap -- '\(.*\)' EXIT/\1/")" EXIT
    # code
    eval "$cleanup"
    eval "$restore_trap"
}
```

which, you know, looks nice and all, but, looking at it right now, are you
convinced it's correct?

Now, what was the point of all this? Surely I did not set out to bash bash...
Oh, yes, parentheses. As a lisp enthusiast, I can appreciate occasions, like
this one, where parentheses shine.

## Subshell awesomeness

Now, say we define our functions with a slightly different syntax:

```bash
my_func() (
    # code
)
```

All we've done is replace the `{}` with `()`. It may look like a benign change,
but now, whenever that function is invoked, it will be _run within a subshell_.
What does that mean? Well, it means that:

- Variables are lexically scoped. You can read variables from the outer shell
  from within a subshell, but you cannot write to them. Variables defined
  within a subshell are invisible to the outer shell. In short, local variables
  within a subshell-function work exactly like you'd expect function-local
  variables to work in most languages. They're also not dynamically scoped.
- Subfunctions work just like you would expect, too: functions defined within a
  subshell are local to that subshell. They can still see the variables defined
  in the outer shell, but not modify them (again, lexical scope). This nesting
  works on as many levels as you need, provided you keep using parentheses to
  define your functions.
- Subshells are, as the name suggests, running in a subshell. They don't
  strictly have to be OS subprocesses (because it can be more efficient for
  them not to be), but they have to behave as if they were. This means that
  they get their own `EXIT` trap if they need one. When the subshell exits,
  normally or due to a crash, it will run its own `EXIT` handler, then pass on
  its exit code to the parent, which may at that point still run its own `EXIT`
  handler. It all nests as you'd want.

## Conclusion

Given all that, I simply do not understand why people keep recommending the
`{}` syntax _at all_. It's a rare case where you'd want all the associated
issues. Essentially, the only "advantage" of _not_ running your functions in a
subshell is that you _can_ write to global variables. I'm willing to believe
there are cases where that is useful, but it should definitely not be the
default.

[man]: https://www.gnu.org/software/bash/manual/bash.html#Shell-Functions
