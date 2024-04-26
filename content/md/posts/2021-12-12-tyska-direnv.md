{:title "Tools You Should Know About: direnv"
 :layout :post
 :tags ["tyska"]}

### In a nutshell

From [the homepage]:

> `direnv` is an extension for your shell. It augments existing shells with a
> new feature that can load and unload environment variables depending on the
> current directory.

There's really not much more to it: it's a very simple feature, but I've found
it tremendously useful and so I wanted to spread the word. At this point,
setting up `direnv` is pretty much the first thing I do when working on a new
project, and I wouldn't want to work without it.

### Why you should know about it

In my experience, managing poject-dependent configuration has always been a bit
of a pain. That is, until I discovered `direnv` a couple years ago.

Environment variables are definitely not the only way to manage configuration
(it's just the best one), but there are many tools that rely on them, and many
more that offer them as an option. Maybe you need to set a library path, or a
locale, or a project-global version for some dependency in a multi-language
project. Maybe you just want all your scripts to know the path to the project
root. Maybe you want to set your Docker daemon coordinates. Maybe you have a
`bin` folder in the some project and you'd like to have that in `PATH`, but
only when you're working on that project.

Whenever a problem could be solved by setting an environment variable, `direnv`
can help. More importantly, if you need to set different sets of variables
across different projects, it can quickly become error-prone. `direnv` solves
that.

### Feature highlights

`direnv` is a very simple tool, but it still packs a punch.

#### The `.envrc` file

The core of what `direnv` does is based on the `envrc` file. `direnv` extends
your shell to check the current path (and its ancestors) for a file called
`.envrc`, and if there is one, it loads it (see below if you're concerned about
the security aspect of that).

That file is expected to mostly set environment variables, and `direnv` will
unset them when you `cd` out of that directory (and its children).

```plaintext
$ cat myproject/.envrc
export MY_VAR=in-project
$ export MY_VAR=outside
$ echo $MY_VAR
outside
$ cd myproject
direnv: loading /myproject/.envrc
direnv: export ~MY_VAR
$ echo $MY_VAR
in-project
$ cd ..
direnv: unloading
$ echo $MY_VAR
outside
$ cd myproject
direnv: loading /tmp/myproject/.envrc
direnv: export ~MY_VAR
$ export MY_VAR=1
$ echo $MY_VAR
1
$ cd ..
direnv: unloading
$ echo $MY_VAR
outside
$
```

#### Security

`direnv` will not just randomly execute any code anywhere: it works with an
allowlist system. Whenever it encounters a `.envrc` file, it only runs it if
this exact content for this exact path has been authorized. If not, it will
print out a warning to signal that there is an `.envrc` file but it has not
been loaded. Adding the file to the allowlist can be done by running `direnv
allow`, which will also immediately load it.

```plaintext
$ cd myproject
direnv: error /tmp/myproject/.envrc is blocked. Run `direnv allow` to approve its content
$ direnv allow
direnv: loading /tmp/myproject/.envrc
direnv: export ~MY_VAR
$ echo $MY_VAR
in-project
$
```

Obviously in general one should take a look at the file before allowing it.

#### Private values

A common idiom is to end the `.envrc` file with a line that looks like:

```plaintext
source_env_if_exists .envrc.private
```

What this means is: if the `.envrc.private` file exists, load it. As the name
suggests, the `.envrc.private` file is generally meant for values that should
not be shared, and that file should therefore not be committed.

This can range from credentials to IDE configuration and the like. And the
variables set in `.envrc.private` will get loaded and unloaded in the same way
as the rest of `.envrc`, meaning this makes it super easy to manage different
sets of credentials across different projects.

### Conclusion

That's about it. A tool doesn't have to be super complicated to be tremedously
useful.

[the homepage]: https://direnv.net
