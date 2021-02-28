{:title "Merge sort in Bash"
 :layout :post
 :tags ["bash" "unix"]}

Upon reading [my previous post], a friend decided to try and write an
implementation of [merge sort] in Bash, using subprocesses and file
descriptors, to further his understanding. When he told me about it, I thought
it was a great idea and gave it a try myself. In this post, I'll present a few
of the things I learned in the process.

### Fifos

Because of the non-linear recursive structure of merge sort, it is not possible
to express it using only anonymous pipes (the `|` operator presented in [my
previous post]). We need one process writing to two pipes, and another process
reading from the same two pipes.

The unix answer to this kind of use-case is known as a [named pipe]. A named
pipe is created by the `mkfifo` command, which takes as argument the name (i.e.
filesystem path) of the pipe to create. A named pipe can then be deleted using
the usual `rm` command (it's just a file). As the name of the command to create
them suggests, named pipes can also be referred to as fifos, for their "first
in first out" behaviour.

Fifos are blocking: when one process tries to write a line to a fifo, it is
blocked until there is another process ready to read from the fifo. Similarly,
if a process tries to read from a fifo, it will block until either the fifo is
closed or there is something to read.

A fifo is closed for reading when there is no process that currently has the
fifo open for writing. This will be important. Note that this means a fifo can
be closed and then reopened.

### Bidirectional file descriptors

In [my previous post], I mentioned the bidirectional file descriptor syntax,
`<>`. For the recursive case of a merge sort, we need to create a child process
that writes to two pipes, and then two child processes that read from those
same pipes.[^1]  It would seem natural at first to create a bidirectional file
descriptor for the fifo, which could then be passed down to all three
subprocesses.

However, this does not work. Bidirectional file descriptors are a smell in
general as they probably indicate a convoluted data flow, but they also suffer
from an issue that ends up breaking this use-case: they can never be closed.

In our case, this means that the process reading from the fifo will end up
blocked on a read that never comes.

### Creating file descriptors in the current process

In [my previous post] I explained how to create file descriptors when launching
a subprocess. Those file descriptors are then visible to the subprocess.
However, sometimes you may want to create file descriptors in the current
process, especially when working with fifos.

Recall that a fifo is closed when there is no (more) process with an open file
handle on it. So if you write something like this:

```bash
mkfifo my_fifo
for i in $(seq 1 5); do
    echo $i > my_fifo
done
```

you end up opening _and closing_ a file handle to `my_fifo` five times. This
means that if you have a process reading on the other end, and it happens to be
faster than you, it may observe a closed fifo in-between two of your writes.
This will likely lead to the reading process declaring it's done and shutting
down, and you will end up being blocked on your next iteration, waiting forever
for a reader that has already moved on.

In this specific case, you can fix the issue by opening the file at the level
of the `for` loop:

```bash
mkfifo my_fifo
for i in $(seq 1 5); do
    echo $i
done > my_fifo
```

and in many other cases you can work around the issue by surrounding your code
in parentheses and using a subshell in the same way:

```bash
mkfifo my_fifo
(
    for i in $(seq 1 5); do
        echo $i
    done
    echo finished
) > my_fifo
```

This can get a bit cumbersome, though, and sometimes you really just want to
say "keep this open until the end of the current script, or until I explicitly
close it". For these cases, you can use the `exec` syntax:

```bash
mkfifo my_fifo
exec 3>my_fifo
for i in $(seq 1 5); do
    echo $i >& 3
done
echo finished >& 3
```

As it will obviously very quickly become very cumbersome to manage file
descriptors as numbers manually, you can (if you're not stuck in the stone age,
wink macOS) use the following syntax:

```bash
mkfifo my_fifo
exec {var}>my_fifo
for i in $(seq 1 5); do
    echo $i >& $var
done
echo finished >& $var
```

At first glance it may seem equivalent to just doing `exec 3>my_fifo; var=3`,
but the above syntax has the added benefit that Bash will pick a new, unused
file descriptor for you.

Note that this can also be used to replace the default file descriptors:

```bash
exec 2>/var/log/my_program/$$/stderr
```

would redirect the stderr of the current process (and therefore all
subprocesses, as they inherit it by default) to the given path. (`$$` is a
special variable that expands to the current pid, which should avoid
clobbering if you run multiple instances at the same time.)

Note that because by default when you open a new shell you start with `stdin`,
`stdout` and `stderr` all set to the terminal, using this in a live shell may
lead to weird-looking behaviour.

### Debugging subprocesses

As a natural extension of the above, you can add a processing step to your
default outputs. For example:

```bash
exec 2> >(sed 's/^/err: /' >&2)
```

redirects the stderr of the current process (and its child processes if they do
not explicitly overwrite it) to a new process that itself redirects its
standard output to the original standard error of the parent process, while
applying a `sed` filter that adds `err: ` at the start of each line.

So if you have a Bash script that calls itself and/or other Bash scripts,
starting all of the files with something like:

```bash
#!/usr/bin/env bash

set -euo pipefail

exec 2> >(sed "s/^/$$ - /" >&2)
```

will give you debug output that lets you easily track the nesting of
subprocesses, along the lines of:

```plaintext
40245 - 40252 - 40256 - message
```

at the cost of running one extra subprocess per subprocess (the `sed` process
in this example).

### Show me the code already!

Alright. I will first explain each chunk of code one at a time, but I will
provide the entire code at the end of this post for reference.

But first, a fair warning:

> **WARNING**: This code has been written to further my understanding of Bash
> processes. You should absolutely not run it for any reason ever; if you need
> to sort something, use the [sort] utility.

This code is extremely inefficient: it will create, per line to sort:
- 2 named fifos
- 2 anonymous pipes
- at least 9 processes that I can count, though some of them short-lived

On a sufficiently long input, this code has pretty much the same runtime
behaviour as a [fork bomb].

### Output

We start with some output configuration. This code is very easy to get wrong in
a way that just blocks without outputting anything, so it's very useful to have
some debugging output.

```bash
# format a line of log output
format () {
    while IFS= read -r line; do
        if [ -n "${DEBUG:-}" ]; then
            # for Bash 3, replace $BASHPID with $$
            echo "${1:-}[$BASHPID] - $line"
        fi
    done
}

# debug output
debug () {
    echo $@ >& 2
}
```

This expects every function to start with `exec 2> >(format >&2)`, and will
essentially give us dynamic scoping on our log prefix. The `DEBUG` flag is used
to enable these logs, because they can be quite noisy.

On Bash 3 (macOS), `$BASHPID` is not available. In modern Bash versions,
`$BASHPID` gives you the pid of the current Bash process. This is different
from `$$` in a few cases, most notably for our purposes here in the case of a
subshell, where `$$` keeps referrring to the parent pid.

### Splitting the input

The first useful piece of a merge sort is the split function, which takes a
list as input and outputs two sublists of the same size.[^2] In our case, the
input and both outputs are given as file descriptors opened respectively for
reading and writing.

Note that this code explicitly expects file descriptors 3 and 4 to be used for
output, and 0 to be used for input. This is fine here because the script is
small enough and is in control of all file descriptors in play. If the script
were more complex it may be worth using the `{var}>` syntax and passing the
file descriptors as arguments.

Speaking of arguments, we may also note that this function does not take any
argument as it gets all of its inputs through side-effects and hard-coding, and
generates its outputs as side-effects. This will be true of subsequent
functions too. However, it is worth pointing out that because this function is
designed to be called in a subshell, the hard-coded file descriptors do not
consitute _global_ hardcoded values.

```bash
split () {
    exec 2> >(format "split" >&2)

    debug "start"
    local item
    local out=0
    while read -r item; do
        debug "put $((out+3)): $item"
        echo $item >& $((out+3))
        out=$(( ! out))
    done
    debug "end"
}
```

This is a fairly simple loop; `(( ! out ))` uses Bash boolean logic to
alternate between `0` and `1`. Remember that this will block on each attempt to
write a line, which is why it needs to be run as a separate process.

### Merging two sorted lists

The next piece of a merge sort is a function to merge two sorted lists. This is
done by reading one element on each side, comparing them, outputting the
smallest and replacing it (in the comparison pair) with the next one from the
same list. Once we reach the end of one of the lists, we can just copy the
other one over.

The Bash code reads almost like the above paragraph:

```bash
merge () {
    exec 2> >(format "merge" >&2)

    debug "start"
    local left right
    read -r -u 3 left || left=""
    read -r -u 4 right || right=""
    until [ -z "$left" ] || [ -z "$right" ]; do
        debug "comparing '$left' and '$right'"
        if [[ "$left" < "$right" ]]; then
            echo $left
            read -r -u 3 left || left=""
        else
            echo $right
            read -r -u 4 right || right=""
        fi
    done
    if [ -z "$left" ]; then
        echo $right
        cat <& 4
    else
        echo $left
        cat <& 3
    fi
    debug "end"
}
```

The notation:

```bash
read -r -u var || var=""
```

is used to ensure that, if `read` returns a non-zero value, presumably
indicating a closed fifo, the result of the line is still a success (avoiding
[the `-e` behaviour]) while also ensuring that the `var` variable is set (to an
empty string, but still set, avoiding [the `-u` behaviour]).

This does mean that this code uses the empty string (identified by `[ -z "" ]`)
as the sentinel value indicating the end of a list, and therefore means that we
cannot sort a list that contains empty strings. I can live with that.

The use of double brackets `[[ ]]` lets us avoid having to escape the
smaller-than comparison operator at the cost of portability. The `sh` version
of the same comparison would be written `[ "$left" \< "$right" ]`.

### Main function: setup

Now that we have the two subcomponents of a merge sort, we can work on the main
function. First, because we're plugging our pieces together through
side-effects, we need to do some setup.

```bash
    local left_pipe=$(mktemp -u)
    local right_pipe=$(mktemp -u)
    trap "rm -f $left_pipe $right_pipe" EXIT
    mkfifo $left_pipe $right_pipe
```

The `mktemp` utility creates files with randomly generated names meant as
temporary storage (it does not, itself, take care of making them temporary,
i.e. it does not delete them). By default, it creates files under `/tmp`, which
gets cleaned up upon reboot. The `-u` flag runs it without side effects, i.e.
it will generate a random file path under `/tmp` but will not create the
corresponding file. `mkfifo` can then be used to create the two pipes we are
going to need.

`trap` is used to catch, and react to, [Unix signals]. Bash introduces an
additional "virtual" signal called `EXIT`. The syntax is pretty simple:
```bash
trap cmd signal
```
will evaluate `cmd` upon receiving `signal`. The `EXIT` signal is generated any
time the Bash process exits, regardless of its status code, provided the Bash
process has time to run its handler (i.e. it will run if a subprocess crashes
and `-e` is set, or upon successful completion, or upon `CTRL-C`, or when
otherwise receiving a `TERM` signal, or when explicitly calling `exit`, but not
when receiving a `KILL` signal).

Importantly, it will be called upon termination of a subshell in modern Bash
versions, but not on the 3.2 Bash version shipped with macOS.

Also note that `trap` is, syntactically, a normal Bash command, so in this case
the first argument is fully expanded before being passed to `trap`, and will be
further evaluated (as in `eval`) upon receiving the signal. In this case the
distinction does not matter, because `left_pipe` and `right_pipe` are never
reassigned, but in some cases it may be important to use single quotes instead,
to defer the variable substitution to the time the signal is handled. As the
string will be `eval`'d, you can also construct more complex commands (chaining
commands with `;`, using functions, etc.).

Finally, it is worth noting that the `trap cmd signal` call overwrites any
previous handler for the `signal` signal. The `EXIT` handler defaults to
nothing so in this case it does not matter, but in more complex scripts it
could. You can print the current handler with `trap -p EXIT`, which you can use
to add to the exit behaviour:

```
trap "rm -f new_file; $(trap -p EXIT)" EXIT
```

or to restore the previous behaviour later on:

```bash
restore_trap=$(trap -p EXIT)
trap "echo 'custom behaviour'" EXIT
do_stuff
trap "$retore_trap" EXIT
```

Finally, you can reset the handler to the default "do nothing" behaviour with
the syntax `trap - EXIT`.

### Main function: base case

The main function is a recursive function, and thus needs a base case to avoid
an infinite loop. The base cases for merge sort are the empty list and
singleton lists, both of which are considered as already sorted.

```bash
    local first second
    if ! read -r first; then
        debug "base case: empty"
        exit
    fi
    if ! read -r second; then
        debug "base case: return $first"
        echo $first
        exit
    fi
```

If the first read fails, we are in the empty list case and immediately exit
with no further output. The `EXIT` handler will take care of removing the
fifos, and the process exiting process will take care of closing any remaining
open file handler.

If the first read succeeds but the second one fails, we are in almost exactly
the same case, except that we need to print the one element we read before
exiting.

### Main function: recursive case

If we have at least two elements, we are in the recursive case and will defer
to the `merge` function to sort them out.

```bash
    cat | (cat <(echo $first) <(echo $second) - | split 3> $left_pipe 4> $right_pipe) &
    merge 3< <(msort < $left_pipe) 4< <(msort < $right_pipe) &
    # for bash 3, replace line above with line below
    #merge 3< <(./msort.sh < $left_pipe) 4< <(./msort.sh < $right_pipe) &
```

Only two lines here, but a lot to unpack. First off, in the case of Bash 3
(macOS), as explained previously the `EXIT` handler is not called for subshells
and thus we will leak many fifos. The simplest workaround is to make the
recursive calls explicit subprocesses instead of simple subshells. This will
also make the `$$` variable work as expected as a replacement for `$BASHPID`.
If you are bothered by the dependency on the file's own name, you can use `$0`
instead of `./msort.sh`.

On the first line, we first need to reconstruct the original input. To that
end, we use `cat` (the nested one) with three inputs: two simple `echo`
subshells and `-`, which means "read from your stdin". Note that the order of
arguments matters to `cat`: if we put the `-` first, `cat` would output its
stdin first, and tack on our two first inputs afterwards. (Which would not
change the ultimate result in this case as this is still unsorted data.) The
output of that `cat` command is processed by the `split` function, for which we
set up the file descriptors 3 and 4 to write on the two fifos.

We then use another `cat` process (the outer one this time) to redirect our own
`stdin` to the `stdin` of the subshell. Finally, we instruct Bash not to wait
on the subshell by using the asynchronous operator `&`.

On the next line, we first set up two subshells that open the two fifos for
reading, processes their content through a recursive call, and then use _their_
outputs as the two inputs to a merge call.

Note that when writing this code, I first wrote it as:

```bash
    cat | (cat <(echo $first) <(echo $second) - | split 3> >(msort < $left_pipe) 4> >(msort < $right_pipe) &
    merge 3< $left_pipe 4< $right_pipe &
```

At first glance, this may seem equivalent. However, this version does not work.
The reason for that is the order of operations: in this version, the two
`msort` subprocesses are created first, before `split` has had time to start
writing on the pipes. This means that (subject to race condition, hence with
some measure of flakiness) when the `msort` processes first try to read from
their input, they may see it as closed and immediately assume that they are in
the "empty list" base case. When, a fraction of a second later, the `split`
process connects, it will then try to write to those pipes and hang forever.

When setting up a pipeline based on fifos, always setup the writers first.

### Main function: clean-up

Because we have started asynchronous children, there is a chance the current
process may finish before them, resulting in orphan processes. To avoid this,
we can wait for our subprocesses to properly terminate:

```bash
    for job in $(jobs -p); do
        debug "waiting on $job"
        wait $job
    done
```

Note that this will only wait on processes that have not finished before we
reach this point, so counting those `waiting` lines is not a good way to count
all the processes this creates.

### Full code

Finally, here is the full code in all its glory:

```bash
#!/usr/bin/env bash

set -euo pipefail

# format a line of log output
format () {
    while IFS= read -r line; do
        if [ -n "${DEBUG:-}" ]; then
            # for Bash 3, replace $BASHPID with $$
            echo "${1:-}[$BASHPID] - $line"
        fi
    done
}

# debug output
debug () {
    echo $@ >& 2
}

# Reads stdin one line at a time and outputs alternatively on fd3 and fd4.
split () {
    exec 2> >(format "split" >&2)

    debug "start"
    local item
    local out=0
    while read -r item; do
        debug "put $((out+3)): $item"
        echo $item >& $((out+3))
        out=$(( ! out))
    done
    debug "end"
}

# Reads lines from fd3 and fd4, which are expected to be sorted, and outputs a
# unified sorted list of lines on stdout.
merge () {
    exec 2> >(format "merge" >&2)

    debug "start"
    local left right
    read -r -u 3 left || left=""
    read -r -u 4 right || right=""
    until [ -z "$left" ] || [ -z "$right" ]; do
        debug "comparing '$left' and '$right'"
        if [[ "$left" < "$right" ]]; then
            echo $left
            read -r -u 3 left || left=""
        else
            echo $right
            read -r -u 4 right || right=""
        fi
    done
    if [ -z "$left" ]; then
        echo $right
        cat <& 4
    else
        echo $left
        cat <& 3
    fi
    debug "end"
}

# Reads in a list of lines on stdin and outputs the sorted list on stdout.
msort() {
    exec 2> >(format >&2)

    local left_pipe=$(mktemp -u)
    local right_pipe=$(mktemp -u)
    trap "rm -f $left_pipe $right_pipe" EXIT
    mkfifo $left_pipe $right_pipe

    local first second
    if ! read -r first; then
        debug "base case: empty"
        exit
    fi
    if ! read -r second; then
        debug "base case: return $first"
        echo $first
        exit
    fi
    debug "recursive case"
    cat | (cat <(echo $first) <(echo $second) - | split 3> $left_pipe 4> $right_pipe) &
    merge 3< <(msort < $left_pipe) 4< <(msort < $right_pipe) &
    # for bash 3, replace line above with line below
    #merge 3< <(./msort.sh < $left_pipe) 4< <(./msort.sh < $right_pipe) &

    for job in $(jobs -p); do
        debug "waiting on $job"
        wait $job
    done

    debug "done"
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    msort
fi
```

These last three lines allow the script to be loaded as a library (`source
msort.sh`) without triggering any side effect. I can't think of a good reason
to do that with this particular script, but I think it's a good practice
nonetheless.

And here is how you can test it:

```plaintext
$ echo hello | ./msort.sh
hello
$ cat /usr/share/dict/words| shuf | head -10 | ./msort.sh
Cantabrize
Christophany
Cordaites
Isthmia
bacteriform
betrayal
circumambulator
divination
flannelbush
hexanitrate
$ cat /usr/share/dict/words | shuf | head -200 | ./msort.sh | wc -l
200
$ cat /usr/share/dict/words| shuf | head -200 | tee >(sort > unix_sort) | ./msort.sh > my_sort
$ diff my_sort unix_sort
$
```

As previously mentioned, this is an extremely inefficient program, so I would
not recommend trying it with many more lines than 200. Note that `diff` prints
all the differences, so the empty output here means we have sorted it the same
way.

And, finally, a run with `DEBUG` output enabled:

```plaintext
$ cat /usr/share/dict/words | shuf | head -4 | DEBUG=1 ./msort.sh
[71104] - 71103 - recursive case
[71104] - 71103 - waiting on 71108
[71104] - merge[71118] - 71103 - start
[71104] - split[71122] - 71103 - start
[71104] - split[71122] - 71103 - put 3: dividend
[71104] - split[71122] - 71103 - put 4: confabulation
[71104] - split[71122] - 71103 - put 3: penwomanship
[71104] - split[71122] - 71103 - put 4: tinglass
[71104] - split[71122] - 71103 - end
[71104] - 71103 - waiting on 71110
[71104] - [71119] - 71103 - recursive case
[71104] - [71120] - 71103 - recursive case
[71104] - [71119] - 71103 - waiting on 71132
[71104] - [71120] - 71103 - waiting on 71133
[71104] - [71120] - merge[71154] - 71103 - start
[71104] - [71119] - merge[71157] - 71103 - start
[71104] - [71119] - split[71159] - 71103 - start
[71104] - [71120] - split[71161] - 71103 - start
[71104] - [71120] - split[71161] - 71103 - put 3: confabulation
[71104] - [71120] - split[71161] - 71103 - put 4: tinglass
[71104] - [71120] - split[71161] - 71103 - end
[71104] - [71119] - split[71159] - 71103 - put 3: dividend
[71104] - [71119] - split[71159] - 71103 - put 4: penwomanship
[71104] - [71119] - split[71159] - 71103 - end
[71104] - [71120] - 71103 - waiting on 71136
[71104] - [71119] - 71103 - waiting on 71137
[71104] - [71120] - [71153] - 71103 - base case: return confabulation
[71104] - [71119] - [71152] - 71103 - base case: return dividend
[71104] - [71119] - [71158] - 71103 - base case: return penwomanship
[71104] - [71119] - merge[71157] - 71103 - comparing 'dividend' and 'penwomanship'
confabulation
[71104] - [71120] - [71160] - 71103 - base case: return tinglass
[71104] - merge[71118] - 71103 - comparing 'dividend' and 'confabulation'
dividend
[71104] - [71120] - merge[71154] - 71103 - comparing 'confabulation' and 'tinglass'
penwomanship
[71104] - merge[71118] - 71103 - comparing 'dividend' and 'tinglass'
[71104] - merge[71118] - 71103 - comparing 'penwomanship' and 'tinglass'
[71104] - [71119] - merge[71157] - 71103 - end
[71104] - [71119] - 71103 - done
[71104] - [71120] - merge[71154] - 71103 - end
[71104] - [71120] - 71103 - done
tinglass
[71104] - merge[71118] - 71103 - end
[71104] - 71103 - done
$
```

[^1]: They need to be processes because of the blocking behaviour of named
  pipes.
[^2]: Where "same size" means "size differing by at most one", for the case
  where the input list does not have an even number of elements.

[my previous post]: /posts/2021-02-21-bash-redirections
[merge sort]: https://en.wikipedia.org/wiki/Merge_sort
[named pipe]: https://en.wikipedia.org/wiki/Named_pipe
[fork bomb]: https://en.wikipedia.org/wiki/Fork_bomb
[the `-e` behaviour]: /posts/2021-01-17-bash-set-dash-e
[the `-u` behaviour]: /posts/2021-01-24-bash-set-dash-u
[Unix signals]: https://en.wikipedia.org/wiki/Signal_(IPC)
[sort]: https://man7.org/linux/man-pages/man1/sort.1.html
