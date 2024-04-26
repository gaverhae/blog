{:title "Safer Bash: no unset"
 :layout :post
 :tags ["bash"]}

[Last week], I talked about `set -e`; while it does add a layer of security on
many potential Bash errors, it is unfortunately not quite enough by itself. In
the next few posts, I'll explore a few other things you can do to improve the
safety of your Bash scripting.

Note that I am not advocating writing complex Bash scripts. There is a point
(around 10 lines, maybe?) where you really need to be asking yourself whether
Bash is the right language, and whether you wouldn't be better served by a more
modern programming language. [Python], [Ruby], [Haskell] (via [stack]),
[Clojure] (via [lein exec]) etc. all have facilities to replace Bash scripts by
providing both a nice API to run subprocesses and an easy way to turn source
files into executables. It's likely your favourite programming language has
similar facilities. Still, in some environments Bash remains the best option.

The additional safety features I am suggesting in this series of posts are not
things you should add when your Bash scripts get complicated; they are things
you should just always add to all your Bash scripts, no matter how small they
seem.

With this caveat out of the way, let's move on to our topic of the week. By
default, Bash, being a dynamic language, essentially behaves as if unknown
variables were set to the empty string. So, for example:

```shell
$ cat hello.sh
set -e

YOU="world"

echo Hello, $you
$ bash hello.sh
Hello,
$ echo $?
0
$
```

(By the way, in case you're wondering, `echo $?` prints the [exit code] of the
last command, which is an integer between 0 and 255 with 0 meaning success and
all the other values being error codes.)

As you can see, this succeeds despite the typo (lack of capitalization). Just
like last week, this may not seem like a big deal. However, let's consider this
[silly example that would never happen in the real world]:

```bash
set -e

MY_APP_FILES=/tmp/my-app

rm -rf $My_APP_FILES/
```

Guess what happens? Since there is a typo in `My_APP_FILES`, Bash will happily
substitute an empty string for it. This means the command becomes `rm -rf /`,
a.k.a. "please delete all the files on my hard drive". If this script is run as
root, your machine is likely to become unable to boot, which can be annoying
for, say, a remote server with no physical access.

However, don't go thinking you're safe as long as you're not running scripts as
root. All things considered, breaking your OS files is a best-case scenario
these days, as they are super easy to replace. What's a lot worse about the
above is that `rm` itself does not stop on error (and, at least on my system,
does not have an option to). So if you run this as a normal user, the command
will fail to delete system files, but will keep going until it finds files it
_can_ delete, for example all your baby pictures you have no backup of.

The solution to this is another flag we can set to tell Bash to stop on
encountering any unset variable. The flag is `-u`, and just like the `-e` flag,
it can be set both when starting the executable and from within a script:

```shell
$ cat unset.sh
set -e

MY_VAR="some value"

echo my_var: $MYVAR

set -u

echo my_var again: $MYVAR

echo Done
$ bash unset.sh
my_var:
unset.sh: line 9: MYVAR: unbound variable
$ bash -u unset.sh
unset.sh: line 5: MYVAR: unbound variable
$ echo $?
1
$
```

Just like `-e`, it would be annoying to require all users of our script to add
the flag to all Bash invocations, and the flag only affects whatever happens
after the `set` line. So you should really start all your Bash scripts with
this line:

```bash
set -eu
```

Like many Unix utilities, Bash allows you to combine single-letter flags, such
that `set -u -e` is the same as `set -eu` (and the same as `set -e; set -u`).


[Last week]: /posts/2021-01-17-bash-set-dash-e
[Ruby]: https://www.ruby-lang.org/en/
[Haskell]: https://www.haskell.org
[stack]: https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter
[Clojure]: https://clojure.org
[lein exec]: https://github.com/kumarshantanu/lein-exec
[Python]: https://www.python.org
[exit code]: https://en.wikipedia.org/wiki/Exit_status
[silly example that would never happen in the real world]: https://github.com/valvesoftware/steam-for-linux/issues/3671
