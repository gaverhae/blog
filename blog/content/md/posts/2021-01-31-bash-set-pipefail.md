{:title "Safer Bash: fail on pipes"
 :layout :post
 :tags ["bash"]}

Over the past two blog posts, I have explained why I always start my Bash
scripts with [`set -o errexit`] and [`set -o nounset`], respectively
abbreviated as `set -e` and `set -u`. They can be combined as `set -eu`. In
this final "mandatory Bash flags" post, I will explain the last option I always
set on my Bash script. After the shebang line, I always type this line:


```bash
set -euo pipefail
```

This is a shorthand for the equivalent

```bash
set -o errexit
set -o nounset
set -o pipefail
```

As I have already explained the first two flags ([`errexit`] and [`nounset`]),
this post will explain `pipefail`. Simply put, this flag instructs Bash to fail
when a pipe fails. What's a pipe?

One of the most useful innovations of Unix was the "everything is a file"
approach to IO. Pipes build upon that by presenting cheap, easy to use virtual
files that are designed to be written to by one program while simultaneaously
being read from by another program. This is easier to explain with a concrete
example.

First, terminal programs that "write to the terminal" are actually, as far as
they're concerned, writing to a file (because everything is a file). This file
is commonly referred to as `stdout`, which is short for "standard output", or
the default place where the output of the program should go. Programs also have
a "standard input", which is where they read input from. In many cases, this
defaults to whatever is being typed in the terminal, because that makes writing
interactive programs easy.

But because input and output are treated, at the API level, as just writing to
and reading from normal files, we can easily _redirect_ them and, without
changing anything to a program, have it read its input from a file rather than
an interactive user session.

For example, the `bc` program is a simple interactive calculator:

```shell
$ bc
bc 1.06
Copyright 1991-1994, 1997, 1998, 2000 Free Software Foundation, Inc.
This is free software with ABSOLUTELY NO WARRANTY.
For details type `warranty'.
1 + 1
2
5 * 18
90
^D
$
```

In this snippet, `1 + 1`, `5 * 18` and `^D` are typed interactively by the
user. But because everything is a file, we can also _redirect_ the "standard
input" file reading operations of the `bc` program to read from a file. In
Bash, this is done using the `<` character. For example:

```shell
$ cat input_file
1 + 1
6 * 32
$ bc < input_file
2
192
$
```

The code of the `bc` program itself does not need to know about the
redirection (though in all fairness it _can_, and in this case it does, and
uses that knowledge to decide not to print the welcome banner).

Pipes are a special kind of file that allows the output of a program to
directly serve as the input of another program. In a Bash shell, they are
written with the `|` character (generally pronounced "the pipe character",
appropriately enough). Why would that be useful? Here is a simple example.

The `du -h` command will list all the files in the current directory along
with their size in "human-readable form", which in this case means using
appropriate units (k, M, G, etc.). It normally prints that listing to the terminal, which means it actually writes it to its "standard output" file.

The `sort -h` program will read its input all the way to the end and then
reprint it in ascending order; the `-h` option means it considers the first
word on each line as a number in the same "human-readable form" as `du -h`
(with no option, `sort` is plain alphabetical order on whole lines).

Finally, the program `tail -10` will read all of its input, and reprint only
the last ten lines.

Consequently, the Bash command

```bash
du -h | sort -h | tail -10
```

will print the ten biggest files under the current directory (along with
their size).

Unfortunately, the default behaviour of Bash is to ignore failures in pipe
chains:

```shell
$ cat failed_pipe.sh
set -eu

echo "This works as it should" | tr a-z A-Z
echo "This should fail" | (cat; mistyped_command) | tr a-z A-Z
echo "We don't want to see this"
$ bash failed_pipe.sh
THIS WORKS AS IT SHOULD
THIS SHOULD FAIL
failed_pipe.sh: line 4: mistyped_command: command not found
We don't want to see this
$
```

Despite us setting `-e`, this does _not_ fail on the `mistyped_command`, but
instead keeps going. (Here, the `(cat; mistyped_command)` form is using a
_subshell_ to simulate a program that fails after having produced a little
bit of output.)

A mental model for understanding why this is not failing is to think of `-e`
as checking the state of the last command after each "full" command, i.e.
roughly after each line. In this case, `mistyped_command` is just one part of
the command, and while that one part fails, the overall line succeeds,
because the return value of a chain of pipes is the exit code of its last
component. In this case, `tr a-z A-Z` succeeds so the entire line is
considered a success.

To prevent this, we need to set the `pipefail` flag. This will use the return
code of the first failed command in a chain of pipes as the return code for
the entire chain.

```shell
$ cat pipe_failing.sh
set -euo pipefail

echo "This works as it should" | tr a-z A-Z
echo "This should fail" | (cat; mistyped_command) | tr a-z A-Z
echo "We don't want to see this"
$ bash pipe_failing.sh
THIS WORKS AS IT SHOULD
THIS SHOULD FAIL
pipe_failing.sh: line 4: mistyped_command: command not found
$
```

You may notice that this still printed `THIS SHOULD FAIL`. This is, in
general, unavoidable: pipes are constantly passing data through, so `tr` gets
the output of `cat` before the subshell fails on `mistyped_command`.


[`set -o errexit`]: /posts/2021-01-17-bash-set-dash-e
[`errexit`]: /posts/2021-01-17-bash-set-dash-e
[`set -o nounset`]: /posts/2021-01-24-bash-set-dash-u
[`nounset`]: /posts/2021-01-24-bash-set-dash-u
