{:title "Bash redirections"
 :layout :post
 :tags ["bash"]}

### File descriptors

An open file requires a "file descriptor", which is a special kind of data
structure stored at the kernel level and mapped to some integer value for
processes that have access to it.

For various reasons (i.e. performance), file descriptors are stored in
specialized, fixed-size data structures, which is why it's important to
remember to close files when you're finished with them.

Traditionally, unix processes are spawned with at least three open file
descriptors: 0 is the standard input (a.k.a. `stdin`), 1 is the standard output
(a.k.a. `stdout`), and 2 is a secondary output stream meant for error reporting
(a.k.a. `stderr`). Most processes are created using a variation on the `fork`
system call, which copies all file descriptors into the new process. In the
case of simple Bash commands, this means that by default the `stdxx` of a
command you run is the `stdxx` of the current Bash session (which is generally
what you want for interactive programs).

### Pipes

The most common form of redirection is the pipe: `|`. The expression

```bash
proc_a | proc_b
```

will create a special 'pipe' file, then start process `proc_a` with that file
as its standard output and process `proc_b` with that file as its standard
input. Note that the `stderr` of `proc_a` is not redirected, which means it is
the same as the parent's (the Bash process that runs this command). For
example:

```shell
$ (echo "this goes to stdout"; echo "this goes to stderr" >&2) | tr [:lower:] [:upper:]
this goes to stderr
THIS GOES TO STDOUT
$
```

As you can see, only the text sent to `stdout` is piped through the `tr`
command (which in this case turns any lowercase letter into the corresponding
uppercase letter). If you're wondering about the `>&2` form, you're in luck, as
that is what the next next section is about. Don't skip the next one, though.

### Redirecting to a file

Pipes are great for chaining programs together, but they don't get rid of the
ephemeral nature of standard input and outputs. Sometimes, it's nice to save
the output of a command to file, or to drive a program from an existing file
rather than having to type things out.

The most common file redirections are `>` and `<`, which will, in their naked
form, respectively redirect `stdout` and `stdin` to/from the given file. We can
illustrate this with the `rev` program, which reads its standard input one line
at a time and prints it to its standard output in reverse:

```shell
$ rev
hello
olleh
this is not a palindrome
emordnilap a ton si siht
$ rev > out
hello
this is not a palindrome
$ cat out
olleh
emordnilap a ton si siht
$ <out rev
hello
this is not a palindrome
$ rev < out
hello
this is not a palindrome
$
```

Note that, while it does not matter to Bash, it is usually considered better
form to put the redirections after the command.

The `>` and `<` commands actually take arguments that make them a lot more
versatile than you might think at first. Obviously, from the examples above,
they take an argument to their right, which is a path to the file you may want
to open. There are, however, a couple variations here.

First off, it's probably better to think of them as "open for reading" and
"open for writing" than as "redirect input" and "redirect output". They both
can take another argument, to their left, to indicate which file descriptor
they are opening; `<` just happens to default to 0 and `>` to 1. So you could
start a process by making its file descriptor 0 (`stdin`) write-only (`echo
"hello" 0>file`), or its file descriptor 1 (`stdout`) read-only (`echo "hello"
1<file`). Neither of these works in most circumstances because most programs
are written under the assumption that they can read from 0 and write to 1. So
that's not very useful so far.

This syntax is, however, useful in redirecting the `stderr` of a program. If
you recall from the introduction of this post that `stderr` is file descriptor
2, you can now understand the notation `2>error.log` as meaning "start this
program with file descriptor 2 pointing to the file `error.log` in write-only
mode". Quite frankly this is by far the most common use of this "first
argument" of the `>` and `<` "redirections" (and the only one I have ever
used), but I can imagine scenarios where opening other file descriptors may
work, assuming the program you are running is designed to expect, say, a
special file on file descriptor 3, e.g. `3>/tmp/trace_level_log` or something.

Finally, it is worth noting that the `>` operator will truncate the given file
if one already exists. This means that any existing content in the file is
lost. If you want to instead append to an existing file, you can use `>>`
instead.

### Redirecting to a file descriptor

It is sometimes convenient to map a file descriptor to another, existing one.
The syntax for this uses the `&` symbol followed by a number instead of a file
name. For example, `2>&1` will redirect `stderr` on `stdout`. Note that this is
actually done by cloning the file descriptor for `stdout` (and possibly making
the result write-only if it wasn't already); it is not "piping" anything
written to file descriptor 2 through to file descriptor 1.

This is important because it means that further modifications of file
descriptor 1 are not propagated to file descriptor 2. Witness:

```shell
$ (echo "stdout"; echo "stderr" >&2) >out_first 2>&1
$ (echo "stdout"; echo "stderr" >&2) 2>&1 >out_second
stderr
$ cat out_first
stdout
stderr
$ cat out_second
stdout
$
```

You can see that, in the second case, because `stdout` is changed _after_
`stderr` has been set, `stderr` goes to the "old" `stdout`, i.e. the terminal
instead of the file.

Note that the same syntax works for read redirection too, i.e. `4<&7` would
create a file descriptor 4 as a clone of the existing file descriptor 7 (but
read-only), but I have never had a need for that. Also, if using a `-` instead
of a number to indicate which file descriptor to copy, this will close the file
descriptor to the left (such that accessing it is an error):

```shell
$ echo hello 1>&-
bash: echo: write error: Bad file descriptor
$ echo hello 2>&-
hello
$ echo hello 0<&-
hello
$
```

Because `echo hello` does not try to write to `stderr` or read from `stdin`,
closing them is not an issue. Closing `stdout`, however, does make it crash.

Note that you can create file descriptors that are not used by the program,
with no other adverse effect than an open file descriptor that won't be closed
until the end of life of that process. (Remember, file descriptors are a
precious resource.) This can be used, for example, to swap `stdout` and
`stderr`:

```shell
$ (echo stdout; echo stderr >&2) 3>&1 1>&2 2>&3 | sed 's/std//'
stdout
err
$
```

In this case we have created file descriptor 3 just to hold the data that was
associated with `stdout` so we can swap `stdout` and `stderr`.

At this point, you may be wondering: if `>` creates a write-only file
descriptor and `<` creates a read-only one, wouldn't it also be useful to have
a way to create a read-write file descriptor? If so, you're in luck. Sort of.
Bash does have a way to create a read-write file descriptor, using the `<>`
operator, which takes the same arguments using the same syntax as the other
two. I've never had a use for it, though, so I'm not entirely sure about how
useful it is.

Finally, because the form `>file 2>&1` is so common, there is a shorthand for
it: `>&file` (where `file` is not a number nor a dash). Or `&>file`; both are
equivalent.

### Subshells as files

Because everything is a file, we can use an entire subshell as a file
descriptor. This is the same idea as a pipe, except that the pipe is strictly
defined as connecting the file descriptor 0 of a process onto the file
descriptor 1 of another process.

Many programs use other files than the three default ones. As a very simple
example, the `cat` command takes a file name and displays its contents:

```shell
$ cat out
line 1
line2
line3
$
```

In any situation where you need a file to pass into a program for reading, you
can substitute a subshell using the syntax `<()`:

```shell
$ cat <(echo "hello" | tr e z)
hzllo
$
```

Obviously the benefit of using `cat` in this way is limited, but hopefully you
get the idea. This also works for output redirection, using the `>()` syntax.
For example, the `tee` command will copy its `stdin` to its `stdout` as well as
to any number of file names given as arguments. This is very useful for
extracting intermediate logs from long pipe expressions. Here is an example of
using `tee` to illustrate the `>()` syntax:

```shell
$ echo "hello" | tee >(cat) >(cat | tr [:lower:] [:upper:]) log > out
hello
HELLO
$ cat log
hello
$ cat out
hello
$
```

The first `hello` output line is the result of the first argument to `tee`,
namely `>(cat)`. The second output line, `HELLO`, is the result of the second
file `tee` writes to: `>(cat | tr [:lower:] [:upper:])`. Finally, the third
argument of `tee` instruct it to write to the `log` file, while the `stdout` of
`tee` is redirected to the `out` file.

### HERE documents and inline strings

Bash can also turn plain strings into input files. The `<<<` notation will pipe
a string through to `stdin`; for example:

```shell
$ bc
bc 1.06
Copyright 1991-1994, 1997, 1998, 2000 Free Software Foundation, Inc.
This is free software with ABSOLUTELY NO WARRANTY.
For details type `warranty'.
1+3
4
^D$ bc <<< "1 + 3"
4
$ echo "1 + 3" | bc
4
$
```

As you can see, piping the `1 + 3` expression using `echo` is the same same as
creating it directly as `stdin` using `<<<` and (minus the banner) also the
same as just typing it manually as interactive input to the program.

If your string is a bit longer, you can use what is known as the "HERE
document" notation, for reasons that will hopefully become clear soon enough.
The syntax starts with the symbol `<<` followed by a word, and ends with the
same word alone on a single line. The word used is arbitrary, but `HERE` and
`EOF` are the most common ones I've seen.

```shell
$ tr [:lower:] [:upper:] <<HERE
> first line
> second line
> third line
> HERE
FIRST LINE
SECOND LINE
THIRD LINE
$
```

There are a couple things to note about HERE documents. First, the document
actually starts on the next line. So you can have more content on the original
line:

```shell
$ tr [:lower:] [:upper:] <<HERE | sed 's/I/A/g'
> first line
> second line
> third line
> HERE
FARST LANE
SECOND LANE
THARD LANE
$
```

Second, by default, the HERE document behaves like a double-quoted string,
meaning you can use Bash variables and subshells within it. If that is not what
you want, you can surround the end word with single quotes:

```shell
$ var="replace me"
$ cat <<HERE
> var: $var
> HERE
var: replace me
$ cat <<'HERE'
> var: $var
> HERE
var: $var
$
```

### Honorable mention: /dev/null

This is not a redirection feature, but it is often used _with_ redirections, so
I think it is worth mentioning. On any unix system, there is a special device
file called `/dev/null` that will accept any write and just discard it
immediately. This is useful when you are running a program and only care about
a subset of its (possibly many) output streams. For example:

```shell
$ du -hs /* 2>/dev/null | sort -h
```

In this case, because we are trying to collect information about `/`, it is
very lilely there will be files `du` cannot read. Normally, it would print a
line on `stderr` for every such line. However, in this case, I don't really
care about that and I accept that the final result may not be entirely accurate
due to such errors.

If you had a program that requires multiple files to write to, you could also
use it in combination with the subshell redirection feature:

```shell
$ ./annoying-program --info-logs /var/log/keep-this \
                     --debug-logs >(cat > /dev/null) \
                     --trace-logs >(cat > /dev/null)
```

Other useful devices are `/dev/random` and `/dev/zero`, which will both accept
any read request and respond to it with, respectively, random bytes and zeroed
bytes.
