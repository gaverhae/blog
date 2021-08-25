{:title "Removing a directory"
 :layout :post
 :tags ["unix"]}

Around the middle of January, my boss asked me if I could look into deleting a
folder on our CI machines. To this day I have not been able to do it. Sounds
interesting? Wondering how hard that can be? Read on.

### Permission refused

On the face of it, this is not a difficult assignment: `rm -r` _should_ do the
trick. So I started there, and got:

```shell
$ rm -r folder
rm: Cannot remove 'folder/file': Permission denied
$
```

Right, fine. There's a flag for that: `-f`. Let's try again:

```shell
$ rm -rf folder
rm: Cannot remove 'folder/file': Permission denied
$
```

Uh? Ultimately this will be running on our CI servers, under an unprivileged
user. However, as this is a local test machine (set up to mirror our CI nodes),
I can try the most violent approach I know of: `sudo`.

```shell
$ sudo rm -rf folder
rm: Cannot remove 'folder/file': Permission denied
$
```

This is getting interesting. Let's take a look:

```shell
$ ls -l folder
total 0
-rw-rw-r-- 1 vagrant vagrant 0 Mar 14 14:21 file
$
```

Uh? The file is not even readonly? This took me a while to figure out. Here's a
hint:

```shell
$ ls -la folder
total 8
dr-x------ 2 vagrant vagrant 4096 Mar 14 14:21 .
drwx------ 3 vagrant vagrant 4096 Mar 14 14:21 ..
-rw-rw-r-- 1 vagrant vagrant    0 Mar 14 14:21 file
$
```

> Obviously this is not a screenshot from the actual issue in January. This is
> me reproducing the exact error that was there, because now I understand it
> well enough to do that. Local testing was indeed done on a Vagrant machine,
> but the actual folder had about a hundred gigabytes of data and _lots_ of
> subdirectories like this one.

It turns out you can't delete files from a read-only directory, which I guess
makes sense. What I would not have expected is `rm -r` to get stuck on that.

### Interlude: legal mentions

We all love legal fine print, right? I'm no lawyer, so I don't know if this is,
strictly speaking, necessary, but just to be on the safe side I will mention
here that all of the code snippets in this blog entry are © [Digital Asset]
Switzerland GmbH.[^1] They are used here under the Apache 2 license.

If you too want to work on cool problems in an open-source-friendly company,
we're [hiring people for my team]. Please reach out to me if you want an intro.
Other teams are also hiring; see [career opportunities] for a full listing.

### Some context

Let's take a step back from our specific need to delete a directory so that I
can explain the actual problem we are trying to solve: our CI machines had
recently started to fail, toward the end of the day, with various incarnations
of `no space left on device` errors.

Our CI machines are completely rebuilt every night, so it is not too surprising
that this would happen more towards the end of the day. It was an easy issue to
ignore when the issue happened around 10pm, but when it started happening as
early as 2pm it became a bit more pressing to solve.

Our inital solution was simply to manually kill affected machines. This is
obviously an imperfect approach as we have no good way to detect when machines
get into that state.

At this stage (early January, though that's actually still true as of this
writing) our best way to detect that a machine has gotten into a state where
we need to kill it is that someone notices and tells me (or another of the
handful of people with permissions to kill CI machines).

I usually monitor `main` branch builds, but I can't monitor all builds across
all PRs, and there are many other, unrelated reasons for PR builds to fail
(like the actual code in the PR being broken).

We considered a few alternatives before arriving at the conclusion that we
should just check current disk space before starting a build and do some
cleanup if there is insufficient space:

- Running only a single build per machine, i.e. killing the machine after each
  build and constantly create new ones, is not practical with our current setup
  because it takes about 20 minutes to create a machine, and even after that they
  get faster as they process more builds and warm up their local cache.
- Emptying the cache after each build leads to the same issue of having to
  delete that folder, but also renders the cache meaningless. We do get a lot
  of mileage out of the cache being somewhat filled up, as long as it doesn't
  fill the hard drive to the point of failing builds.
- Killing the machine once we reach this "disk full" state is not practical,
  because we have not found a good way to do that without leaving Azure
  Pipelines, our CI "manager", in a broken state if the machine disappears while
  processing a build.
- [Bazel], the process actually responsible for filling up our hard drives,
  does have a command specifically meant to clean its cache, `bazel clean
  --expunge`. However, this doesn't work in our case for a couple of reasons.
  First, Bazel has like four different kinds of cache and that command only
  cleans up one of them. Second, we have multiple Bazel workspaces on our CI
  machines (because they run multiple builds/projects), and that command only
  cleans up the cache for the current Bazel project. We want to clean all of it.

How did we know the disk was filled by the Bazel cache and not something else?
Essentially connecting to the CI machines that had reached that state and
recursively running:

```shell
sudo du -hs * 2>/dev/null | sort -h
```

starting at `/` and digging down until we found an identifiable directory (or
handful of directories) that took up most of the space. `du -hs *` will print
lines that look like

```
16k home
4g etc
```

meaning that the `etc` folder in the current directory takes up 4 GB and the
`home` folder takes up 16 KB (that's obviously a made-up example), while `sort
-h` is able to take lines that start with such "`h`uman-readable" numbers
(using units like `k`, `m`, `g`, etc.) and sort them in ascending order,
meaning the biggest folders end up at the end where they're visible even if
`du` spit out a large number of lines.

### find | xargs chmod

So back to deleting that Bazel cache folder. We now know that the issue is
permissions on the subfolders, not the files themselves, so the immediate
solution I came up with was to [change permissions on all subfolders]:

```bash
local_cache=$HOME/.cache/bazel
if [ -d "$local_cache" ]; then
    if [ $(du -ms "$local_cache" | awk '{print $1}') -gt 80000 ]; then
        echo "Deleting '$local_cache'..."
        find "$local_cache" -type d | xargs chmod +w
        rm -rf "$local_cache"
    else
        echo "Local cache small enough:\n$(du -hs "$local_cache")"
    fi
else
    echo "No '$local_cache', moving on."
fi
```

The relevant part is this line, which follows very naturally from our
conclusions so far: find all the directories and make them writable.

```bash
find "$local_cache" -type d | xargs chmod +w
```

If you've ever tried to use `find` and `xargs` together, you should immediately
spot the mistake here: what if we have spaces? Surely Bazel is well-behaved
enough not to use spaces in its internal cache structure? It is, but only most
of the time. So sometimes this script crashes. So this script is an
improvement, as when it does work we can keep using the machine a bit longer,
but it does end up failing and then Bazel refuses to run as it considers its
cache corrupted. So we're back to manually killing machines, albeit a little
bit less often.

I've played with various ways to try and unquote arguments on the `xargs` side
properly. I've learned quite a bit about how `xargs` can construct commands
using `{}` and the like. Somehow, I realized a bit later that I had completely
forgotten about the `0` option:

```bash
find . -print0 | xargs -0 cmd
```

instructs `find` to separate files using an actual zero character (i.e. ASCII
code 0, not the character for `0` which is ASCII 48), and `xargs` to use that
same zero character as the field delimiter, so that would have taken care of
the parsing. Ultimately, though, before I remembered the `-print0` option, I
realized that I don't actually need to use `find` at all in this case and
[replaced that line with]:

```bash
chmod -R +w "$local_cache"
```

This technically does a little bit more work as it will also make the files
writeable, but that's really not an issue here as we're going to delete them
anyway.

This succeeded most of the time[^3], but it had one big drawback: it took about
an hour and a half to run. Given that our typical CI run takes between 20 and
30 minutes, most people would actually prefer the build to fail so they can
reschedule it on another machine rather than wait for that long.

### Need for speed

So I added a couple `echo $(date -Is)` commands to the script in order to
figure out where all that time went. It turns out computing the size of the
cache took about twenty minutes already, so that was a good first place to
improve. Thinking about it, it's not that surprising that it would take time
for `du` to traverse hundreds of thousands of files, get their size from the
file system, and then sum that to get a total.

Surprisingly, `ncdu` was able to give me the total size of that folder in a
very small fraction of the time. I did not dig into why `du` was so much
slower.

So [I replaced the "cache size" check with a "free space" check]:

```bash
if [ $(df -m . | sed 1d | awk '{print $4}') -lt 30000 ]; then
```

which is practically instant.

This is a lot better, but it still leaves us  with about an hour of runtime.
Googling around for ways to delete a large directory quickly did not bring up
much at first. I eventually found a suggestion to use `rsync` instead of `rm
-rf`. When I tried that locally, however, I ended up with timings along the
line of 30 seconds for the `chmod` call, about one minute for the `rm -rf`
call, and about 5 minutes for an equivalent `rsync` call (which, interestingly
enough, doesn't need the `chmod` step). That did not look very promising.

(Obviously my local runs were done with a much smaller cache.)

### Mounting files

More googling eventually led me to an interesting suggestion: why not mount a
separate file system on the cache folder? That way, when we want to clean up, we
can just unmount it and mount a new one. No need to spend any time at all
looking at the existing files in that old file system. Even more googling lead
me to discover that you can create a file system within a normal file and then
mount that. On the face of it:

```bash
#/usr/bin/env bash

set -euo pipefail

file=/tmp/cache_file
mount_point=/home/vsts/.cache/bazel

if [ -d "$mount_point/lost+found" ]; then
    umount $mount_point
    rm -f $file
fi

mkdir -p $mount_point
truncate $file -s 200g
mkfs.ext2 -N 6000000 $file
mount $file $mount_point
chown -R vsts:vsts $mount_point
```

should solve our problem. It took a lot of googling to stumble on the different
pieces used in this script, so I'll walk through the interesting lines.

First, the `lost+found` folder will only[^2] exist if the file system has
already been mounted, so the `if` conditional is a guard that lets this script
succeed both when run for the first time and when run subsequently to reset the
cache.

The `truncate` command is really cool: if the given path does not exist, it
will create a _sparse_ file at the given path with the given size. In this
case, we create a 200GB file that takes up no size at all on the parent file
system (which incidentally has a _total size of 200GB_) and will grow as needed
when we start writing on it. This is perfect for our purposes as we actually
have more than one cache folder and this will allow us to create a separate
file system for each one without needing to guess how to preallocate space. As
these virtual file systems grow, total free space on the host partition will
gradually go down, and our existing test relying on that remains valid.

The `mkfs.ext2` command creates a file system; it is generally used against a
block device (`/dev/sdx1`), but it turns out it works just as well against a
file. The `-N` argument here ensures we get enough inodes, as Bazel is very
hungry for those (remember, the main reason we need to do all this in the first
place is that Bazel creates _lots_ of _small_ files).

We use `ext2` here because, according to my research at least, we don't need
the features added in later versions:
- `ext3` adds journaling, which we explicitly don't want here: our CI machines
  are never rebooted (if they shut down we consider them dead and create new
  ones), so the recovery use-case doesn't exist, and we're specifically looking
  for speed manipulating large numbers of small files.
- `ext4` adds support for "very large" files, which is completely the opposite
  of what we plan to use this file system for.

Finally, `mount` is just as happy mounting a file as it would be mounting a
block device, so that works out very well for us.

There's just one problem with this script: it only works if you run it as root.
We don't want our CI jobs to have root access on the machines, for very many
security reasons. The usual answer to "I just need to run that one command as
root" is `sudo`, specifically with a restricted entry in the `/etc/sudoers`
file. However, this doesn't really work here as that command would need to be
`mount`, and if you can run `mount` as root you're bascially root on the host
system.

The solution I came up with was to use the `setuid` permission bit. You can set
it by running `set 4xxx file`, and what it does is allow the file to be run as
the owner of the file (rather than the user running the command). So, for
example, if we have the above script as `remount.sh`, and it had the correct
permissions:

```shell
# chown root remount.sh
# chmod 4775 remount.sh
```

we could run it as a normal user:

```shell
$ ./remount.sh
```

and it would execute as root. Except it doesn't.

### Let's write some C

The obvious reaction to that `setuid` idea is "that sounds dangerous". It
really is: lots of privilege escalation issues have relied on `setuid` in the
past. For that reason, modern unices forbid (or rather ignore) the `setuid` bit
on scripts. It only works on "proper" executables. So, [let's convert that Bash
script into C]:

```c
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int exists(const char* path) {
    struct stat s;
    return stat(path, &s) == 0 && S_ISDIR(s.st_mode);
}

int execute_f(char** args) {
    int status;
    char** child_env = { NULL };
    pid_t pid = fork();
    if (0 == pid) {
        // child process
        if (-1 == execve(args[0], args, child_env)) {
            printf("Failed to start child process calling %s.",
                   args[0]);
            perror(0);
            exit(1);
        }
        // unreachable: if execve succeeded, we're off to another
        // executable
    }
    // parent
    if (-1 == pid) {
        printf("Failed to fork.");
        perror(0);
        exit(1);
    }
    if (-1 == waitpid(pid, &status, 0)) {
        printf("Failed to wait for child process '%s'.", args[0]);
        exit(1);
    }
    if (1 != WIFEXITED(status)) {
        printf("Child not terminated: '%s', aborting.", args[0]);
        exit(1);
    }
    if (0 != WEXITSTATUS(status)) {
        printf("Child '%s' exited with status %d",
               args[0],
               WEXITSTATUS(status));
        exit(WEXITSTATUS(status));
    }
    return 0;
}

#define execute(...) execute_f((char*[]) { __VA_ARGS__, NULL })

int main() {
    char* mount_point = "/home/vsts/.cache/bazel";
    char* lost = "/home/vsts/.cache/bazel/lost+found";
    char* file = "/root/cache_file";
    setuid(0);
    if (exists(lost)) {
        printf("Removing existing cache...");
        // This will fail if there are dangling processes, say Bazel
        // servers, with open fds on the cache.
        execute("/bin/umount", mount_point);
    }
    execute("/bin/rm", "-f", file);
    // truncate creates a sparse file. In my testing, for this use-case,
    // there does not seem to be a significant overhead.
    execute("/usr/bin/truncate", "-s", "200g", file);
    execute("/sbin/mkfs.ext3", file);
    execute("/bin/mount", file, mount_point);
    execute("/bin/chown", "-R", "vagrant:vagrant", mount_point);
    return 0;
}
```

This script is a lot simpler than it may seem at first. We literally try to do
what Bash does, i.e. all the work is done in subprocesses. The `exists`
function uses direct system calls to check if a file exists, but everything
else is done through `execute`. This is a macro because that was the nicest
syntax I could come up with to automatically ensure that all my calls ended in
a `0` (`NULL`), so the arglist is properly terminated as per the [execve(2) man
page]. Apart from that, the `execute_f` function is mostly error handling, and
in our case all we want to do is make sure we detect if an error happened and
crash.

So my bright idea is to write C code to make things _more secure_. That
obviously sounds counter-intuitive. I believe the above C code is safe because
it does not take any input at all: there is no file reading, no argument
parsing, and every single string is defined at compile time. In short, there is
no opportunity for any kind of overflow. The `execve` form used specifically
resets the environoment, so there is also no opportunity to trick any of the
subprocesses through that channel. It seems to me that the only way you could
subvert this code is by having taken over one of the executables it calls. As
all those executables are owned by root, that would mean you're already root so
you don't really need another vector for privilege escalation.

My colleagues, however, were not impressed. While nobody could point to a
specific issue in this code, everyone was a bit uneasy about running my C code
as root on sensitive machines, especially given that this is quite literally
the very first C program I have written professionally. While I would have
liked to be able to say I wrote C code that runs as root in production, I
really can't blame them here.

### fuse

A colleague suggested using [FUSE] instead. Simply put, FUSE is exactly what I
want here: it's a way for normal, unprivileged users to mount file systems. It
was mainly developed for the pluggable media use-case (cd-roms, USB sticks,
etc.), but does have a file-backed implementation.

So [I tried that]. Using FUSE let me keep pretty much the same Bash script, but
with `fuse2fs` instead of `mount`. Unfortunately, whenever I tried to run our
Bazel build on a FUSE-mounted file system, the build failed with some weird
permission errors. This seems to indicate some sort of error in the underlying
file system, but debugging that was definitely going to take way more time than
this problem was worth investing in.

At that point, it looked like a dead end. We eventually came to the following
conclusions:

1. This issue is ultimately not all that big of a deal. We've been living with
   it for the past two months now and it has arguably not caused any issue, so
   that seems like it was the right call.
2. I had already spent about two weeks on it, and that's more than we were
   willing to pay to fix what is, after all, an _annoyance_, not a blocking
   bug.
3. As the only two cheap options left at that point were to either use the C
   code or live with the issue, we decided to just live with the issue.

And that's how I failed to delete a folder for more than two months.

### What about user mount?

When I started writing this post, I was fully intending to write more C code.
My boss had mentioned in one of our conversations that he would be willing to
reconsider if the C code were making system calls directly rather than going
through subprocesses. So I spent a good chunk of the day reading man pages for
system calls. While I was looking at the [mount(2) man page], I was scratching
my head about how to tell the system call I wanted the mounted file system to
belong to a given user (`vsts`, in this case, as that's our CI user). I could
not find that information on that page, so I followed the link to [mount(8)],
hoping to find more information there. Instead, I discovered that there is such
a thing as a "user mount".

I'm not quite sure how that escaped me for so long, or why none of my
colleagues suggested it. Now that I've read about it I suddenly remember that I
used to know about it, many years ago when I was working as a sysadmin.

I kind of regret not having a reason to write that C code anymore, after having
done all this research for it. I had gotten as far as being able to write the
code to create the file, truncate the file, delete the file and mount it, and
was about to move on to making the file system.

However, now that I remember user mounts, I can say this is definitely a much
better solution. Maybe you've been screaming at your screen since you started
reading this blog, and you already know all about user mounts. If not, here's
how this works. Anyone can run the `mount` executable; it is, itself, able to
`setuid`, but as it's a very old, battle-hardened binary it is pretty
trustworthy (unlike my own C code). In the general case, if it detects the user
running it is not root, it will exit with an error message. However, before
doing that, it checks the `/etc/fstab` file, and it is possible for that file
to instruct the `mount` program to let users mount _some_ file systems.

I'm not going to reproduce the entire [mount(8)] page here (it does describe
all the options you can put in `/etc/fstab`); for our purposes, we can add this
line to the end of `/etc/fstab`:

```plaintext
/tmp/cache_file /home/vsts/.cache/bazel auto rw,user,exec
```

What does that mean? The first two fields are obviously the device being
mounted and the mount point. The third field is the file system, and here I am
using a value of `auto` to let `mount` auto-detect it. If I wanted to be more
restrictive I could use `ext2` here. The last field is a list of options.

- `rw`, as expected, means that the file system is mounted in read-write mode.
- `user` means that this can be mounted by a normal user, and then unmounted by
  the same user. `user` normally implies `noexec`, which prevents the execution
  of any file from this file system, `nosuid`, which disables the entire `setuid`
  (and `setgid`) mechanism for any executable on this file system, and `nodev`,
  which means that this file system cannot represent block devices (i.e. we can
  only put "regular" files in there).
- `exec` is there to undo the `noexec` implied by `user` and allow executing
  binaries from the file system. This is necessary in our case as Bazel does
  copy binaries in its cache and run them from there.

With `setuid` turned off at the file system level, this seems safe enough for
me. Time will tell if that also seems safe enough to my colleague for them to
accept [my pull request].

[execve(2) man page]: https://man7.org/linux/man-pages/man2/execve.2.html
[Digital Asset]: https://www.digitalasset.com
[hiring people for my team]: https://www.digitalasset.com/careers?gh_jid=2334137
[career opportunities]: https://www.digitalasset.com/careers
[digital-asset/daml]: https://github.com/digital-asset/daml
[change permissions on all subfolders]: https://github.com/digital-asset/daml/commit/cf949b6bae5c7f579178279689bc50e0ca5cc9b2#diff-3ae70bc1a058184397548f16f60d527a1b84038fdb92212d3573401aa49d3110
[Bazel]: https://bazel.build
[replaced that line with]: https://github.com/digital-asset/daml/commit/8350224aefbec68d5bfa4373ac8a4c4122f05d99
[instance group manager]: https://github.com/digital-asset/daml/blob/b0948b417b88fe3a21bdb0df1ab6e56ae3c690a3/infra/vsts_agent_ubuntu_20_04.tf#L14-L32
[I replaced the "cache size" check with a "free space" check]: https://github.com/digital-asset/daml/commit/01f11109fa4ecf1d274e9a38efa23ff1fd31ad98
[`set -e`]: /posts/2021-01-17-bash-set-dash-e
[let's convert that Bash script into C]: https://github.com/digital-asset/daml/pull/8595
[fuse]: https://www.kernel.org/doc/html/latest/filesystems/fuse.html
[I tried that]: https://github.com/digital-asset/daml/pull/8599/files
[mount(2) man page]: https://man7.org/linux/man-pages/man2/mount.2.html
[mount(8)]: https://man7.org/linux/man-pages/man8/mount.8.html
[my pull request]: https://github.com/digital-asset/daml/pull/9137

[^1]: The blog is written in the first person because I actually did the work,
      but the nature of my contract with Digital Asset is such that this code
      belongs to them.

      Because that work also happens to be open-source, I can still use it as a
      reference for my personal blog (this one right here), which is not affiliated
      with, nor endorsed by, Digital Asset in any way. See [digital-asset/daml]
      on GitHub for the full code in which these snippets appeared.

[^2]: Obviously someone could deliberately create it to foil our script. What I
  mean here is that Bazel will not create it, so under normal circumstances
  this is a valid check. If someone does manually create it to foil the script,
  it will just crash on the `umount` line (thanks to [`set -e`]) without doing
  any harm or, as far as I can tell, opening up any opportunity for attack.

[^3]: Occasionally this will fail with an error message along the lines of
  "chmod: cannot read directory /some/path: Permission denied". We are still
  not quite sure what causes this.
