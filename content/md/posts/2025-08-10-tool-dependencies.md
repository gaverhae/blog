{:title "Managing a Project's Tool Dependencies with Nix (and direnv)"
 :layout :post
 :tags ["tyska"]}

For the past six years, I have lived in a world where:

- Projects I work on define a _complete_ list of all of the tools you need to
  work on them, with their exact version.
- The entire description of how to install said tools is `direnv allow`.
- This installation is completely isolated to that one project; there is no
  risk of conflict with other projects or polluting the user's global `$PATH`
  with any of it.
- Yet there is still cross-project caching: if two projects use the (exact)
  same tool, it's only downloaded once.

If you enjoy spending days following manual installation instructions from
a README, only to discover that the listed dependencies cannot be installed
in that way on your machine, or that the README describes an installation
method that leaves the tool globally installed, or that you already have that
tool but another, incompatible version, or that the list of dependencies
described in the README is not complete, or if you're using Windows and can't
stand WSL, this post is not for you.

If, however, my world sounds appealing to you, here's how you can live in it
too.

### Bang for the buck

Astute readers will have gathered from the title that this is achieved through
some combination of [Nix] and [direnv]. If you've heard of Nix before, you may
already be thinking that this is going to be a very hard-to-use setup, because
you're going to have to learn all of Nix and write tons of Nix code to get
anything working.

I, too, used to think that. However, the approach I am promoting here requires
very minimal Nix knowledge. In fact, everything you need to know about Nix is
going to be contained within this post. And with just that, you'll get all of
the above benefits.

### "Tool" dependencies?

Nix maximalists will encourage you to use Nix as your entire
build-and-dependency-management system. This requires a lot of buy-in from the
entire team, and possibly a lot of effort (even if you already know Nix very
well) depending on how well the language you're using is already supported by
Nix.

I am not advocating that. I think in most cases programmers will be most
productive using their language's default set of tools, which generally include
a compiler (or interpreter) as well as a dependency manager. Projects also tend
to grow to require more tools over time (frequent examples include `bash`,
`curl`, `jq`, etc.).

For example, I would use this approach to provide my environment with `npm`,
but I would then use that `npm` to manage my `packages.json` and
`node_modules` etc. in the usual way; I would not recommend trying to use Nix
itself to provide individual Node packages.

The approach I am recommending (and using) is to use Nix to provide those
tools, so you don't need to have them installed "globally". Then, within your
poject, you simply use those tools like you're used to, with the added
confidence that:

- You did not have to research how to install them: Nix takes care of that.
- They are not installed globally, and cannot conflict with other tools in
  other projects.
- You are using the exact same version of these tools as everyone else on your
  team, reducing the chances for subtle version-dependent bugs (or outright
  confusion for some tools where multiple completely different tools of the
  same name exist, such as `yq`).

### The end goal

Here is what I want (and have):

```shell
$ bb --version
zsh: command not found: bb
$ cd project-1
direnv: loading /tmp/project-1/.envrc
direnv: using nix
direnv: export [...]
$ bb --version
babashka v1.3.185
$ cd ..
direnv: unloading
$ bb --version
zsh: command not found: bb
$ cd project-2
direnv: loading /tmp/project-2/.envrc
direnv: using nix
direnv: export [...]
$ bb --version
babashka v1.12.200
$ cd ..
direnv: unloading
$ bb --version
zsh: command not found: bb
$
```

I.e. when I cd into such a project, my shell's `$PATH` is automatically updated
to know about this specific project's dependencies, with the appropriate
versions, no leakage to my "global" user environment, and no conflict between
projects.

### Minimal direnv

For this to work, you need to have [direnv] installed and properly set up,
which in most cases will involve adding one line to your shell configuration
file. In my case, I install it by running `brew install direnv` once per
computer, and I set it up by adding this one line to my `.zshrc`: `eval
"$(direnv hook zsh)"`.

Once that is done, within each project, all you really need is a file called
`.envrc` with the following content:

```
use nix
```

You can put a lot more into that file; it is a Bash script that will be
evaluated when you `cd` into the folder where it appears, and the environment
of your shell will reflect the changes made in that script.

### Minimal Nix

Each user needs to have Nix installed. I have opinions on how to do that, which
I'll come back to at the end of this post. For now, let us assume that you have
done this once-per-user setup step.

Note that, while installing direnv and Nix properly may appear a bit
complicated, once you've done that you can expect you'll almost never need to
install anything manually again, at least as far as programming project
dependencies are concerned. This gives this investment extremely high returns
in my book.

Once you have Nix installed, a project needs a `shell.nix` file, which is the
file that direnv will be looking for with its `use nix` directive. Here is a
minimal `shell.nix` file you may want to start with:

```nix
let
  spec = { commit = "6a489c9482ca676ce23c0bcd7f2e1795383325fa";
           sha = "0vsvkhy3gb8yzq62vazhmpqixssmd4xinnll7w73l4vrqd611wlf"; };
  pkgs = import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${spec.commit}.tar.gz";
           sha256 = spec.sha;
         }) {};
in
pkgs.mkShell {
  buildInputs = [
    # add your dependencies here
    pkgs.babashka
    pkgs.curl
    pkgs.jq
  ];
}
```

This will give you:

```shell
$ bb --version
babashka v1.12.200
$ curl --version
curl 8.14.1 (aarch64-apple-darwin24.5.0) libcurl/8.14.1 OpenSSL/3.5.1 zlib/1.3.1 brotli/1.1.0 zstd/1.5.7 libidn2/2.3.8 libpsl/0.21.5 libssh2/1.11.1 nghttp2/1.65.0
Release-Date: 2025-06-04
Protocols: dict file ftp ftps gopher gophers http https imap imaps ipfs ipns mqtt pop3 pop3s rtsp scp sftp smb smbs smtp smtps telnet tftp
Features: alt-svc AsynchDNS brotli GSS-API HSTS HTTP2 HTTPS-proxy IDN IPv6 Kerberos Largefile libz NTLM PSL SPNEGO SSL threadsafe TLS-SRP UnixSockets zstd
$ jq --version
jq-1.8.1
$
```

Now, there's a little bit more going on here than in the minimal `.envrc` file
above, so let me break it down a bit. First, a very short primer on Nix syntax:

- `let var1 = val1; var2 = val2; in expr` defines local variables. `var1` can
  be used in the expression for `val2`, and all of the defined vars can be used
  in `expr`.
- `{ key1 = val1; key2 = val 2}` defines a "set" in Nix parlance (more commonly
  a map or dictionary in other languages) that associates names to values. Once
  you have defined a set, you can access its elements using dot-notation:
  `spec.sha` accesses the `sha` key in the `spec` set.
- Strings are in quotes, and expressions can be interpolated into strings by
  using the `${expr}` syntax.
- `[val1 val2]` defines a list of two elements. There is no delimiter for list
  items.
- Function application is expressed by space: `f arg` (outside of a list)
  applies the function `f` to the argument `arg`.
- Parentheses can be used for grouping; for example, `[val1 (f arg)]` is a list
  of two elements, the second of which is the result of applying a function `f`
  to an argument `arg`. Without parentheses, we would be looking at a list of
  three elements: `[val1 f arg]`, the second of which happens to be an
  unapplied function.

With that in mind, this minimal `shell.nix` is doing the following:

1. We define a specific commit we want from the [nixpkgs] repository. The
   `commit` entry is the commit SHA as seen by git (and GitHub), whereas the
   `sha` entry is the hash of the downloaded tarball as seen by Nix. This gives
   you some level of security, a discussion of which is outside the scope of
   this post.
2. We fetch the tarball corresponding to that commit, and then evaluate that as
   a Nix expression (`import`). This gives us a list of thousands of packages,
   each with a very specific, pinned-down version that includes not only its
   own version string, but a hash of all of its source code and that of all of its
   dependencies.
3. From that very large set of Nix definitions ("derivations"), we call the
   `mkShell` function, appropriately named as we're trying to make a shell. We
   give it one argument, which is a set with one entry called `buildInputs`. The
   value of that entry is the list of packages we want available in our shell.

In order to use this approach, this is pretty much all you need to know about
Nix.

### Evolving a project over time

Evolving your dependencies will take the form of three operations: adding a
dependency, removing a dependency, or updating your dependencies. I'll tackle
each one in turn.

#### Removing a dependency

This is the easiest one: you just remove the corresponding entry in the
`buildInputs` list.

#### Adding a dependency

If you know the name of the Nix package you want to add, you can just add it to
the `buildInputs` list. If you don't know the name of the package, you can use
the [search.nixos.org] site, where you can search for packages either by
approximate name, or by name of (one of) the executables the package provides.

What if your dependency is not in the package registry at all? Well, at that
point you have a choice to make. Either you delve into Nix a bit more, looking
for existing derivations outside of `nixpkgs` or writing your own, or you
simply install that one dependency separately, and document it in your README.
Just because you're using this system does not mean you lose access to whatever
you were doing before, and you still get all these nice properties for the
tools that _are_ managed by Nix.

Plus, you could test for that tool being on the `$PATH` in `.envrc`, so you
still get some level of automation around it (or, at least, it being missing).

Unless it's an extremely niche thing, though, it's very likely going to be in
`nixpkgs` already.

There is also a chance the package _is_ in `nixpkgs`, but is somehow broken. It
happens; Nix is an open-source effort, after all. In my experience, it's been
very rare, though.

#### Updating your dependencies

In the minimal config we've been looking at, all of your dependencies come from
the same nixpkgs "snapshot". This works well as long as you essentially want
the latest of everything, and you're willing to upgrade everything in a single
step.

Under those assumptions, upgrading your snapshot means updating the `commit`
and `sha` values of the `spec`. You can find the latest commit by looking at
the `nixpkgs-unstable` [branch] of the nixpkgs repo. (You can use other
branches; this one has worked well for me.) Finding the corresponding `sha`
value is a bit more tricky. The easiest way I've found is to put garbage[^sha]
in the `shell.nix` file and try to run it, which will print an error telling
you what the value should be.

[^sha]: The garbage value still needs to look like a valid value to Nix, or you
    will get a formatting error instead.

So the process would be:

1. Go to GitHub to get the latest `commit`.
2. Update the `shell.nix` file to modify the `commit` value, and change the
   `sha` value by overwriting a few characters with 0.
3. Save and quit; `direnv` should detect that the `shell.nix` file has changed
   and try to run it anew; this should produce an error telling you both the
   expected value (the one you just garbled) and the actual value for `sha`.
4. Copy-paste the expected value in the `shell.nix` file.

This process can be automated, but it is a bit more complicated than just
running `sha256sum` on the tarball.

Now, this will update all of your tools, so you should thoroughly test this.
What happens if something goes wrong? Say, for some reason, there is a bug in
`babashka`, but you still want to bump `curl` to get some security update.

Well, there's nothing magical about `pkgs`; you can have more than one. The new
`shell.nix` could be something like:

```nix
let

  # holding back bb while #issue-number gets sorted out; try to upgrade
  # bb again when borkdude fixes it
  # TODO: check again in ten minutes
  spec_old_bb = { commit = "6a489c9482ca676ce23c0bcd7f2e1795383325fa";
                  sha = "0vsvkhy3gb8yzq62vazhmpqixssmd4xinnll7w73l4vrqd611wlf"; };
  pkgs_old_bb = import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${spec_old_bb.commit}.tar.gz";
           sha256 = spec_old_bb.sha;
         }) {};
  # using new commit for everything else
  spec = { commit = "641d909c4a7538f1539da9240dedb1755c907e40";
  # Note the zeroes here:
  #                                       vvvv
           sha = "0vsvkhy3gb8yzq62vazhmpqi0000d4xinnll7w73l4vrqd611wlf"; };
  pkgs = import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${spec.commit}.tar.gz";
           sha256 = spec.sha;
         }) {};
in
pkgs.mkShell {
  buildInputs = [
    # add your dependencies here
    pkgs_old_bb.babashka
    pkgs.curl
    pkgs.jq
  ];
}
```

and this would produce an error that includes these lines (in bright pink, easy
to spot):

```
error: hash mismatch in file downloaded from 'https://github.com/NixOS/nixpkgs/archive/641d909c4a7538f1539da9240dedb1755c907e40.tar.gz':
         specified: sha256:0vsvkhy3gb8yzq62vazhmpqi0000d4xinnll7w73l4vrqd611wlf
         got:       sha256:10hpb1aw884k3zzcy1mhf47dqvfagiyx7kr6hg0p5xcwg04mkx8x
```

which tells us the final form of `shell.nix` should be:

```nix
let
  # holding back bb while #issue-number gets sorted out; try to upgrade
  # bb again when borkdude fixes it
  # TODO: check again in ten minutes
  spec_old_bb = { commit = "6a489c9482ca676ce23c0bcd7f2e1795383325fa";
                  sha = "0vsvkhy3gb8yzq62vazhmpqixssmd4xinnll7w73l4vrqd611wlf"; };
  pkgs_old_bb = import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${spec_old_bb.commit}.tar.gz";
           sha256 = spec_old_bb.sha;
         }) {};
  spec = { commit = "641d909c4a7538f1539da9240dedb1755c907e40";
           sha = "10hpb1aw884k3zzcy1mhf47dqvfagiyx7kr6hg0p5xcwg04mkx8x"; };
  pkgs = import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${spec.commit}.tar.gz";
           sha256 = spec.sha;
         }) {};
in
pkgs.mkShell {
  buildInputs = [
    # add your dependencies here
    pkgs_old_bb.babashka
    pkgs.curl
    pkgs.jq
  ];
}
```

I know that, realistically, by the time we've found the correct value for
`sha`, `bb` is likely already fixed, but it can take some time for updates to
get through to nixpkgs.

### A complete list

I mentioned multiple times that I was confident the list of dependencies in
`shell.nix` was _complete_, yet as I've decribed it so far it's only adding
entries to the `$PATH`[^libs]. Am I too trusting in my and my coworker's
discipline?

[^libs]: Though I haven't used it much myself, as I generally work in
    higher-level, dynamic languages, the Nix shell can also set up native
    libraries in `LD_LIBRARY_PATH` and so on.

Well, definitely not in mine. I forget stuff all the time. But Nix has another
tool that can help us here; when running on CI, or for local testing, you can
run the `shell.nix` file in "pure" mode, where, instead of adding to your
current shell, it creates a new shell that has in its `$PATH` only exactly what
is described in the `shell.nix` file. This way, you don't get any pollution
from your global environment, and you can verify that the set of dependencies
you've defined in `shell.nix` is indeed sufficient to run all of your CI tasks.

You may be wondering why pure mode is not the default. The answer is that there
are things you want on your `$PATH` as a developer that do not make sense as
project dependencies, a big one being your editor.

To start a pure shell using the minimal `shell.nix` above, simply run:

```
nix-shell shell.nix --pure
```

To run commands on CI, you can give that an argument of the command to run, say:

```
nix-shell shell.nix --pure "lein test"
```

On CI, you may need to "activate" Nix. Locally, this will be done through your
shell configuration file, directly by the Nix installer, but on CI you will
typically need to add it yourself, so a running step on GitHub Actions would
for example look like this:

```
    - name: run tests
      run: |
        . /home/runner/.nix-profile/etc/profile.d/nix.sh
        nix-shell shell.nix --pure --run "lein test"
```

You'll obviously also need to have Nix installed in your CI environment
somehow. You generally do not need direnv in CI, though.

### My own tooling

The above should be enough to get you started, perhaps experiment a bit. Some
of the steps are a bit tedious, though. I personally use (and maintain) a
script to help me set up new projects; this script generates the initial files
(`.envrc`, `shell.nix`, and a few others), as well as a script to update the
`nixpkgs` snapshot.

That script is called `init-nix` and is hosted [here][init-nix]. My shell is
set up such that it is always in my (user-global) `$PATH`. This lets me fairly
quickly try out new software with something like:

```shell
$ cd $(mktemp -d)
$ init-nix
$ vim shell.nix
$ direnv allow
$ <new software is available>
```

### Managing the Nix installation

Nix is not really meant to be used in this way. I think. I still haven't really
"seen the light" and "taken the plunge", or [swallowed the pills][nix-pills]. I
mean to, someday, when I have "more time".

Nix is not available on Windows, and I'm a bit unclear on what the BSD
situation is. In my experience, after a bit of friction around the initial
release of Apple Silicon, Nix works really well on both macOS and Linux,
including WSL (though the virtual filesystem can make some operations a bit
slower there).

Nix comes in two flavours: "single-user" and "multi-user". The [Nix]
installation page strongly recommends a multi-user install, touting it as "more
secure". That is likely very true if you are in one of these two situations:

- The computer has more than one active user, so Nix needs to protect against
  interference between users. The vector of attack here would be one user
  poisoning the "Nix cache" (the folder, `/nix`, where Nix stores everything)
  such that, when another user uses some package from there, they get infected
  somehow. If you're on a single-user computer, this concern is vastly reduced.
- You frequently run untrusted Nix code. This happens if you use Nix as your
  build system, or download Nix derivations from random places on the internet.
  Everything you download from the internet should generally be suspect, but, if
  you use Nix in the way I advocate here, i.e. the vast majority of derivations
  you use come straight from `nixpkgs-unstable`, this concern is vastly reduced.

This brings me to my own preference for a single-user install. Most notably:

- The multi-user install requires root access during installation, and sinks
  its teeth fairly deeply into the system, creating multiple users and
  spilling files in various "system" places.
- Uninstalling a multi-user Nix installation is relatively well documented, but
  it is a bit of a hazardous and very tedious task.
- On macOS at least, a multi-user Nix install gets partly wiped out by major OS
  updates, and sometimes by minor ones, which, combined with the previous step,
  is a real pain.

So, my personal preference and recommendation is to go for the single-user Nix
install. If you manually create a `/nix` folder and give your user write access
before running the Nix installation process, you can run the Nix installation
process as your normal user, no `sudo` required. This works very well on Linux,
where creating the `/nix` folder is as easy as `sudo mkdir /nix; sudo chown
$USER /nix`.

The situation on macOS is a little bit more complicated, because:

1. Apple does not generally want users to create new top-level folders.
2. As a consequence of this, the Nix _installer_ does not support single-user
   installs on macOS.

A single-user installation on macOS is still possible, but requires a little
bit of extra, one-time work. I've been running a single-user Nix install on
macOS since 2018 and it's always worked very well; much better than my
colleagues running a multi-user one. Here are the steps you need to take:

1. As root, create the file `/etc/synthetic.conf` with the following content:
   `nix`. Really, just that one line. This tells macOS you want a top-level
   folder called `nix` (i.e. `/nix`); this will be a synthetic path, which
   means it is not a folder and cannot be used directly. It can only be used as
   a mount point for a volume, so on to the next step.
2. Create a new volume, using Disk Utility. It can be part of your main hard
   drive, and, for security-conscious people, can be encrypted in the same way
   the rest of your internal hard drive is. You can call it whatever you want,
   but I called it `Nix`, and that's what I'll use here. If you put spaces in
   the label, you're on your own for the next step.
3. Open up `/etc/fstab` as root, and very carefully add the following line:
   ```
   LABEL=Nix /nix apfs rw,nobrowse
   ```
   It is highly recommended to edit this file with `sudo vifs` rather than
   `sudo <editor> /etc/fstab`, as having a malformed `fstab` can be hazardous.
   Note that `vifs`, despite the name, will honor `$EDITOR`. This tells your
   computer to mount the new volume onto the synthetic `/nix` path.
4. Reboot your computer, double-check you now have a `/nix` path that can be
   written to by the current user.
5. We now have everything we'd need, except that there's a pesky "if multi-user
   and macOS: exit 1" line in the Nix installer. Commenting out the `exit 1`
   line is enough to get it working, as it slides past that and does its work.

There's a bit of spelunking needed here as the official install method for Nix
is of the form `curl ... | bash`. But we can be smart about it and leave out
the `|bash` part to see the actual script. I've done that work, once, and made
it part of my [reset-nix] script. You can take a look at lines 40-49 (as of
writing) to see what that entails:

```
  cd $(mktemp -d)
  curl https://releases.nixos.org/nix/nix-2.10.3/nix-2.10.3-${platform}.tar.xz > tarball
  tar xf tarball
  cd nix-2.10.3-${platform}
  printf '64d\n230d\nw\n' | ed -s install
  ./install --no-daemon
```

The important line here is:

```
  printf '64d\n230d\nw\n' | ed -s install
```

which runs `ed`, the original true `ed`itor, to delete lines `64` then `230`
(so `231` if you look at the file before the removal of `64`). Line `64` is the
aforementioned `exit 1` line; line `231` is the one that adds a line to
`~/.zshrc` to source the Nix files, which I am disabling here because I already
have it. If you run my [reset-nix] script as is, and you're using zsh, you'll
need to manually add this line to your `.zshrc` (once):

```
f="$HOME/.nix-profile/etc/profile.d/nix.sh"; if [ -f "$f" ]; then . "$f"; fi
```

Once you have one Nix installed (here 2.10.3), it's pretty easy to ask it to
upgrade itself by running `nix upgrade-nix`, if you want to. Though, if you're
not getting into Nix more seriously than I suggest here, it's likely you won't
really see any difference between 2.10.3 and more recent versions.

This [reset-nix] script also takes care of another issue that will come up if
you adopt this approach more broadly: garbage collection. If you follow the
approach I have outlined in this post, you will over time accumulate things you
no longer need under `/nix`. I personally run the [reset-nix] script about
every other month.

### Conclusion

I'm hoping you see the value of this approach, and that this post has the right
level of detail to help you adopt it.

If not, please let me know! This approach is unfortunately simple enough that I
don't really see a good way to make it its own GitHub repository, meaning this
page is the only place where I can explain and promote it. Any feedback that
would allow me to make this page better is very welcome, even if it's
thoroughly negative.

I'd really like to see more people adopt something like this, as it's removing
so much unnecessary pain.

[Nix]: https://nixos.org
[direnv]: https://direnv.net
[nixpkgs]: https://github.com/NixOS/nixpkgs
[search.nixos.org]: https://search.nixos.org/packages
[branch]: https://github.com/NixOS/nixpkgs/tree/nixpkgs-unstable
[init-nix]: https://github.com/gaverhae/dotfiles/blob/main/home/bin/init-nix
[reset-nix]: https://github.com/gaverhae/dotfiles/blob/main/home/bin/reset-nix
[nix-pills]: https://nixos.org/guides/nix-pills/
