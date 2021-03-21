{:title "Safer Bash: die on error"
 :layout :post
 :tags ["bash" "unix"]}

Bash is a great programming environment, in the same sense that JavaScript is a
great programming language. That is, it's a pretty terrible design by any
reasonable modern standard, but it's got reach. When it comes to automating
servers, there are still many tasks for which Bash is hard to beat.

Bash does not support modern concepts like exceptions, but it does have _some_
facilities for managing errors. The most important one is probably to not
silently ignore errors. It's great that Bash supports that; it's unfortunate
that it's turned off by default. In most installations, the following runs to
completion:

```shell
$ cat script.sh
does-not-exist
echo "hello"
$ bash script.sh
script.sh: line 1: does-not-exist: command not found
hello
$ echo $?
0
```

Not only did execution not stop on the error, but the script has overall
signalled successful completion. This is in most cases a huge deal: Bash
generally deals with killing processes and deleting files, so you absolutely do
not want to keep going in the presence of unexpected errors. This doesn't
really seem like a big deal in this silly example, but what about the
following?

```bash
do-backup
send-email "admin@example.org" "Backup completed, you can sleep soundly."
```

Believing you have backups when you actually don't can be a Very Bad Thing™.

There is a simple fix for this issue. It's far from covering all of the Bash
safety issues, but it does go a long way: the `-e` flag. Behold:

```shell
$ cat script.sh
does-not-exist
echo "hello"
$ bash -e script.sh
script.sh: line 1: does-not-exist: command not found
$ echo $?
127
$
```

This is a lot better. There is still one small wrinkle, though: you have to
remember to put that `-e` on your Bash invocation every single time. Moreover,
if you share that script with someone else, they also need to know to set that
flag.

Fortunately, Bash has a solution for this too: you can set such flags at any
point from the Bash script itself, and it will turn the flag on from that point
on. So this should really be the first thing you do in every Bash script.

```shell
$ cat script.sh
set -e
does-not-exist
echo "hello"
$ bash script.sh
script.sh: line 1: does-not-exist: command not found
$ echo $?
127
$
```
