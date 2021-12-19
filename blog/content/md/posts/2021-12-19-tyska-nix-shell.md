{:title "Tools You Should Know About: nix-shell"
 :layout :post
 :tags ["tyska"]}

Rcently, I've discovered how to leverage Nix to reap a lot of its benefits with
a _very_ minimal investment, specifically reading this one blog post.

I've been aware of the [Nix] toolset for over a decade now, but until recently
it's always seemed pretty nebulous. Most of the documentation I've come across
explains what the benefits are and how the tooling works in order to deliver
those benefits, but not how to _actually_ use any of it.

Specifically, it always seemed like there was no easy way to get started
reaping at least _some_ of these benefits without a pretty massive learning
investment.

### In a nutshell

In [this series], I usually start with an excerpt from the official homepage.
However, in this case, it's pretty useless for people not already familiar with
Nix (which is my target audience here; if you know how to use Nix already, you
won't learn anything here).

From [the homepage]:

> The command `nix-shell` will build the dependencies of the specified
> derivation, but not the derivation itself. It will then start an interactive
> shell in which all environment variables defined by the derivation path have
> been set to their corresponding values, and the script $stdenv/setup has been
> sourced. This is useful for reproducing the environment of a derivation for
> development.

There you go. Now, let me explain in plainer terms why that's _awesome_ and how
you can leverage it immediately after you finish reading this blog post.

### Why you should know about it

Most software projects have what I'd call "tool dependencies". In many
languages, you have your more explicit "library" dependencies, usually managed
by a language-specific package maanger (gem for Ruby, Maven for Java, cargo for
Rust, npm for JavaScript, etc.). But there's a layer under that: the language
runtime or compiler, the language-specific package manager, all sorts of extra
little scripts most projects tend to grow over time. Perhaps a linter, or some
infrastructure-related tooling. Those are the types of dependencies I'm
concerned with in this post.

In my experience, managing those tooling dependencies is usually left entirely
to individual developers, based on some vague instructions in the README when
you're lucky. This is not great for many reasons:

- Developers need to figure out how to install the various tools the project
  needs, and sometimes even just what those tools are.
- People may be adding dependencies without realizing it: when writing a new
  script, it's easy to accidentally depend on some CLI tool that's present on
  your machine, but maybe isn't on everyone's.
- Most tools are installed "globally" on a machine. For example, on most OSes
  it's awkward to juggle multiple JVMs, or multiple versions of Node, or
  multiple versions of Bash or curl.

Ad-hoc solutions have been invented for that last point, such as rbenv and
nvm. Those tend to be language-specific and only provide you with an easy way
to manage your language installations (respectively Ruby and Node for rbenv and
nvm). They typically don't do anything for other tools, and if you're working
on a multi-language project you may have to contend with multiple such tools.

While that isn't the problem nix-shell was built to solve, it solves it
beautifully. Nix-shell is for you if you like any of these properties:

- You don't want to worry about tooling for one project interfering in any way
  with tooling for other projects.
- You always want to have the expected tools (with their expected versions) for
  every project you work on, with no extra effort expended on your part
  figuring out either what those tools are or how to install them.
- You want to be confident everyone working on a project (for a value of
  "everyone" that covers macOS and Linux users; I'm not aware of any Windows
  support for Nix), _including CI machines_, is using the same set of tools
  down to the specific version.

### Feature highlights

The [Nix] project as a whole has _many_ parts, but this post focuses on one
very specific way of using one of the tools in the (vast) Nix toolbox.
Therefore, there's a single highlight: the ability to get a reproducible
(across time as well as across machines) environment for either running a
script or opening a shell.

### A bit of context

The Nix project aims at building a completely reproducible world. "Nix" is a
reproducible package manager (/ build system), as well as the name of the
(purely functional) programming language used to define the corresponding
"build rules", or "packages", called "derivations" in the Nix world; "NixOS" is
a Linux-based operating system built around the Nix package manager, while the
NixOps project brings the same level of reproducibility to infrastructure
management.

When used outside NixOS, the Nix package manager can run alongside your OS's
package manager (apt, yum, brew, etc.) without interfering with it in any way.
The Nix package manager stores all of its packages under the `/nix/store` path,
and packages under that path are named by the hash of their dependencies.
Transitively, this is what allows Nix to manage the coexistence of many
different versions of the same package, be that different versions of the
source code or the same source code built against different sets of
dependencies. Installing things with Nix is pretty easy; the tricky part is to
get those weird hash-looking paths in your `PATH`. And that's where Nix
tutorials usually want to take over your entire machine so they can manage your
shell.

This is where `nix-shell` comes in. You can live in a world where your default
state is to compeltely ignore your Nix installation, and then only add specific
Nix packages to your `PATH` when working on specific projects.

The other question that this should raise is how do you know what hash to
install in the first place. The answer to that is that Nix works with a
repository of Nix "recipes" called [nixpkgs], which is a GitHub repo that
contains "derivations" for many, many, many programs. The way you decide on
what to install is that you tell Nix to use a specific commit from that repo as
your "package definition", and when things work out as intended all of the
packages defined in a single commit there work with each other.

So instead of having to manually specify a version for each tool you want
available, you specify a nixpkgs revision once, and then just refer to each
tool by its package name.

### How it works in practice

#### Installing Nix

First, you need to [have Nix installed][install]. This is a one-time action,
and on macOS you'll need `sudo` access to do it (you can do a "single-user"
install on Linux without giving `sudo` access to the install script if you
manually create a writeable `/nix` before running the install script).

#### Initializing a project

To initiate a new project where Nix is in charge of your dependencies (or add a
nix-shell configuration to an existing project), run:

```bash
nix-shell -p niv --run "niv init -b nixpkgs-unstable"
```

This will create a Nix configuration in the directory `./nix` that specifies
the revision for `nixpkgs` as the latest commit on the `nixpkgs-unstable`
branch at the time of running that command. (There may be reasons to choose
other branches, but if you're not familiar with Nix, `nixpkgs-unstable` is a
good default.)

Next, we'll want to create a `shell.nix` file, with content that looks like
this:

```plaintext
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.bash
    pkgs.jdk8
    pkgs.terraform
  ];
}
```

This is Nix language for "load the definitions from `nixpkgs`, and build the
`buildInputs` list."

At this point, if you had started in an empty directory, this should look like
this:

```plaintext
$ tree -a
.
├── nix
│   ├── sources.json
│   └── sources.nix
└── shell.nix

1 directory, 3 files
$
```

where `sources.json` and `sources.nix` have been created by `niv`.

And that's pretty much it. You can now run scripts using this environment:

```plaintext
$ nix-shell --run "java -version; terraform -v; bash --version"
openjdk version "1.8.0_292"
OpenJDK Runtime Environment (Zulu 8.54.0.21-CA-macosx) (build 1.8.0_292-b10)
OpenJDK 64-Bit Server VM (Zulu 8.54.0.21-CA-macosx) (build 25.292-b10, mixed mode)
Terraform v1.1.2
on darwin_amd64
GNU bash, version 5.1.8(1)-release (x86_64-apple-darwin17.7.0)
Copyright (C) 2020 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>

This is free software; you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
$
```

You can open up a new (Bash, by default) shell with those three tools loaded in
`PATH`:

```plaintext
$ terraform -v
zsh: command not found: terraform
$ nix-shell

[nix-shell:/tmp]$ terraform -v
Terraform v1.1.2
on darwin_amd64

[nix-shell:/tmp]$ exit
exit
$
```

By default, the `nix-shell` command will _add_ its `buildInputs` to the current
`PATH`, which is usually what you want for local development (so you can still
access your own, personal, project-independant tools like `emacs` or `vim`).
But, particularly on CI, you may want to run some script using _only_ the
`nix-shell`-provided environment so you can make sure it's "complete". You can
do that with the `--pure` option to `nix-shell`, which resets the `PATH` to
_just_ the `buildInputs`, and clears most environment variables. For example:

```plaintext
$ nix-shell --pure --keep GITHUB_TOKEN --run ./my-script.sh
```

would run `my-script.sh` using just the configuration in the local `shell.nix`
file, clearing all environment variables except `GITHUB_TOKEN` (`--keep` can be
specified multiple times to keep multiple env vars).

### `direnv`

`nix-shell` as presented in the previous section would alread be pretty nice,
but combined with [direnv] it's a real game changer. Assuming you have [direnv]
installed, you can add an `.envrc` file to the project above with this content:

```plaintext
use nix

source_env_if_exists .envrc.private
```

to automatically load the `shell.nix` dependencies whenever you `cd` into the
project, and unload them when you `cd` out. If you work on multiple projects
that use slightly different versions of many dependencies, having such a setup
in each of them makes it _really_ easy.

### Updating the `nixpkgs` revision

Besides not having to look up the current commit on the tip of the
`nixpkgs-unstable` branch, using `niv` to manage the `nixpkgs` revision allows
you to later upgrade to the current latest with:

```plaintext
nix-shell -p niv --run "niv update"
```

which will automatically update `nix/sources.json` to point to the latest
version at the time of running.

### Where to find package names

One of the most recurrent problems in my work when setting up machines is to
find out what the name of a package is, when I already have a pretty good idea
of what it should be. Many package managers have some sort of search feature
for that, but for Nix, the way I usually go about it is to check [this
file][all-packages] in the [nixpkgs] repository. The names on the left of the
equality operators are the top-level package names that you can access through
the `pkgs.` prefix in the `shell.nix` file sample above.

### Bonus: `nix-shell` shebang

If you can assume everyone using your project is going to have Nix installed,
but may not be using `direnv` (or you don't want to setup `direnv` on CI, for
example), you can use `nix-shell` as your shebang with a syntax that looks
like:

```plaintext
#!/usr/bin/env nix-shell
#!nix-shell -i bash shell.nix
```

The first line instructs your OS to execute the file with `nix-shell`; the
second line is parsed by `nix-shell` itself and in this case indicates it
should run the file through the `bash` interpreter after having loaded the
environment described in `shell.nix`. You may want to add a `--pure` argument
on that second line in some circumstances.

### Conclusion

Hopefully by this point you know how to use Nix to provide reproducible
toolsets for your projects. My point is not that there is nothing else to learn
about Nix; time invested learning more about it would be well spent. But I hope
to have made the point that you can leverage it very easily, even with little
investment.

[the homepage]: https://nixos.org/manual/nix/unstable/command-ref/nix-shell.html
[Nix]: https://nixos.org
[this series]: /tags/tyska
[install]: https://nixos.org/download.html#nix-quick-install
[nixpkgs]: https://github.com/NixOS/nixpkgs
[direnv]: https://direnv.net
[all-packages]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/all-packages.nix
