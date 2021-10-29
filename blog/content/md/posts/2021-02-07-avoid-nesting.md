{:title "Safer Bash: avoid nesting"
 :layout :post
 :tags ["bash"]}

In the past three blog posts ([`set -e`], [`set -u`], [`set -o pipefail`]), I
have described the header I put as the first line of all my Bash scripts and
why you probably should get into that habit too. As a reminder, here is the
full line:

```bash
set -euo pipefail
```

Together, these three options give you a pretty good (relative to the
out-of-the-box Bash experience, that is) safety net, in that you can expect
most issues with your script to stop the script with an error.

There is, however, a common Bash pattern that can still result in uncaught
errors. Let's start with an example:

```shell
$ cat subshell_failure.sh
set -euo pipefail

echo "Here is the current date: $(dat)."
echo "Success!"
$ bash subshell_failure.sh
subshell_failure.sh: line 3: dat: command not found
Here is the current date: .
Success!
$
```

The issue here is related to why we need `pipefail` in addition to `errexit`:
we can think of `errexit` as checking the return code of each command before
moving on to the next, but this behaviour, however, is not recursive.

In this case, while building the string does not succeed, that happens in a
subcommand, and that subcommand, while failed, still returned _a_ string. That
string is then passed to the `echo` command, and the `echo` command _succeeds_,
therefore avoiding detection by `errexit`.

I am not aware of any flag that can be set to catch this case. Therefore, the
solution is to avoid it in the first place, i.e. to not nest commands further
than you absolutely need to.

A subshell substitution on its own will correctly report an error (and thus
trigger `errexit` if it is set):

```shell
$ cat subshell_caught.sh
set -euo pipefail

CURRENT_DATE=$(dat)

echo "Here is the current date: $CURRENT_DATE."
echo "Success!"
$ bash subshell_caught.sh
subshell_caught.sh: line 3: dat: command not found
$
```

Extracting complex expressions to a named variable is good practice in general
in any programming language, but in Bash, you now have an extra reason to do
it.

[`set -e`]: /posts/2021-01-17-bash-set-dash-e
[`set -u`]: /posts/2021-01-24-bash-set-dash-u
[`set -o pipefail`]: /posts/2021-01-31-bash-set-pipefail
