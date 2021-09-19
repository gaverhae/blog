{:title "The elements of git"
 :layout :post
 :tags ["git"]}

This is not a tutorial. If you're looking for a quick, easy, "how to use git"
kind of post, look elsewhere.

The goal of this post is to give you just enough understanding of the git
internals that you can build up a correct intuition of what various git
commands actually do under the hood.

## The git behind the curtain

`git` is a fraud. You may think it's a complicated piece of code that does a
lot of impressive things. After all, that's the command you invoke all the
time.

It's actually a very simple piece of code: the `git` command only does one,
very simple, thing: it looks at its second argument, `arg`, and invokes
`git-arg` with the rest of the arguments.

There's a bit more to it as it also resolves the path to the proper `git-arg`,
regardless of whether you have all the `git-*` commands in your PATH. But the
point is: git is not one program. It's a collection of different programs,
written by different people and sometimes in different languages, that all work
together pretty seamlessly.

How do they achieve that kind of collaboration? Glad you asked.

## The git data model

The secret is that git is, first and foremost, an on-disk data structure
(hereafter "git repo"). It's very well-defined and documented, which means that
anyone can write a new tool that processes a git repo. This is a great way to
organize software, as it allows each tool to do what it does best, and a
minimal amount of top-level glue (the `git` executable) to tie all of them
together and present a seamless experience.

I believe that [this "data-first" approach][data] is what allowed git to beat
mercurial, by enabling a team completely unrelated to the git project to build
GitHub in a different programming language. By contrast, mercurial was
architected primarily as a single program, with the internal data
representation left as an unstable implementation detail. This gave it some
level of unity in both code and user experience, but made it a lot harder to
build other projects on top of it.

The other nice property of having a suite of tools built around a common data
model is that, if one understands the data model, one can very quickly grasp
what any new tool does with it. This is what the rest of this post is about.

## Objects and refs

The git model is primarily composed of, on the one hand, four types of objects,
and on the other hand, refs to said objects.

As a first approximation, a git repo is a folder with two subfolders: `objects`
and `refs`.

Objects are stored in the `objects` directory following a content-based naming
scheme in which every object is named by the SHA1 of its content.[^sha1] (For
filesystem efficiency reasons, the `objects` folder has 2-letter subfolders
containing the first two letters of the SHA1, and the actual files are named
with the other 38 letters of the SHA1.) In general, objects can have any
content and are immutable: because they are named by their SHA1, if you change
the content, you get a new name, and thus a new object. There may be
circumstances in which you can then delete the old object, but we'll get to
that later on.

[^sha1]: This is a good enough approximation to understand what's going on, but
  if you actually try to `sha1sum` the objects in a git repo, you'll see that
it does not match. The reason for that is that git prepends a header indicating
the type and size of the object when computing the hash, whereas the stored
object itself only contains the content of the object. Also note that this
content is sometimes encoded or compressed; for this blog post we're going to
ignore all that and discuss the conceptual values of objects.

Refs are stored in the `refs` folder. There a semantically-meaningful
subfolders in `refs`, but for now let's just focus on the refs themsevles: a
ref is a text file whose content is exactly one SHA1. It is a `ref`erence to
the git object named by that SHA1.

There are four object types and three ref types; we're now going to take a look
at each of those in order.

### `blob` objects

The simplest type of git object is the blob. A blob is just a delimited stream
of bytes, with no further structure as far as git is concerned. They are
opaque, atomic units of content. In practice, blob objects will represent the
content of files in your git worktree.

### `tree` objects

A tree object represents a directory. It is (conceptually) a simple text file
where each line represents an entry in the directory; entrie can themselves be
of type tree for nested directories, or of type blob for files in the current
directory.

Here is an example of a tree object in the git repo for this blog:

```plaintext
$ git cat-file -p 011c9eb4838434f7e431073feb592fce99f6ad74
100644 blob b1bef1e6634f68e14f3e45f5accc9a6ead0852d5    config.edn
040000 tree c6d62294a2323c2047b43787d55a44ff8c94f565    css
040000 tree 07ef3db049157a829ad3b10f2f6e7936ab5aba47    md
$
```

As previously explained, that file is stored in
`.git/objects/01/1c9eb4838434f7e431073feb592fce99f6ad74`. The file itself is a
compressed representation of an encoding of that text, which is why we're using
`git-cat-file` with the `-p` (pretty print) option to take a look.

Note that the tree does not know its own name: the name of a tree is determined
by the parent. This is good if you have two subfolders with the same content in
different places in your repo (or at different times): if only the name of a
folder changes, all git needs to do is rewrite the parent tree, but the tree
itself and all of its content is completely unchanged.

The same goes for blobs: blobs don't know their own name, so renaming a file
only requires changing the parent tree, not the blob itself. (And recursively
all parent trees, so renaming a file deeply nested down is more "costly",
though that's a negligible cost on most filesystems.)

### `commit` objects

This is the one that ties everything together. Like `tree` objects, `commit`
objects can be thought of as text files. Their structure is a little bit more
complicated, though: they have a number of headers, some of which are optional,
followed by a content. A bit like HTTP requests have a set of headers followed
by a body.

Let's start with looking at an example:

```plaintext
$ git cat-file -p 331c11ee2048a2ec0a1120ccba17085c053eefa0
tree 8a634ffbf8d921e17de4b0202c7c2c289b89e467
parent c00dd30f7eb28d6153b7ea393eca0f79f8cb963b
author Gary Verhaegen <gary.verhaegen@gmail.com> 1632001353 +0200
committer Gary Verhaegen <gary.verhaegen@gmail.com> 1632001353 +0200

blog: copy DNS records from OVH
$
```

We can see four headers here. The `tree` header references a `tree` object
which will serve as the root of the worktree for this commit. The `parent`
header (which can appear any number of times) represents the set of parents for
this commit. In most commits, you'll have one, but merge commits may have two
or more, and the initial commit(s) will have none.

`author` and `committer` are useful metadata; both can appear multiple times.
It's important to realize that they are just text in a text file and thus very
easy to forge. If that is a concern for you, you should [sign your
commits][sign].

Note that git is a blockchain. A blockchain is a succession of "blocks" that
are identified by a hash of [their content _and_ a reference to their parent],
which is how the "chain" is formed.

### `tag` objects

Tag objects have a similar structure to commit objects, but a different set
of possible headers. In particular, tags do not have a parent, and can point to
any type of object. Most often, tags will point to a commit, but you could tag
a specific tree or blob (or, I suppose, a tag).

The git UX makes it sometimes hard to differentiate between tag objects and tag
refs. See the "`tag` ref" section below for more information.

### `branch` ref

Branch refs are text file that contain a SHA1. They can represent either local
or remote branches, respectively under `.git/refs/heads` and
`.git/refs/remotes`. They are generally managed through the `HEAD` special ref.

The "name of the branch" is simply the name of the file. You can have slashes
in a branch name; the file will then be nested in subfolders under
`.git/refs/heads`.

Branch refs always point to a commit.

### `tag` ref

Tag refs are stored under `.git/refs/tags`. They are, by themselves, not
different from branch tags; they differ only in how the special ref `HEAD`
handles them. Tag refs, like tag objects, can point to any type of object.

Note that a tag ref is a simple ref: it does not allow one to associate any
metadata with the tag (beyond its name). You create a tag ref by using the
`git-tag <tagname> <object>` command. If you want to associate extra metadata
to a tag (for example, a signature), you need a tag object, which is created by
the `git tag -a <tagname> <object>` command.

### Special refs

In addition to tags and branches, git keeps track of a number of "special"
refs, directly under `.git`. The most important one is `.git/HEAD`, which
points to the current commit.

Special refs can be direct or symbolic. A direct reference for `HEAD` would
mean that the `.git/HEAD` file contains a SHA1. This is generally not what you
want. A symbolic reference for `HEAD` means that `.git/HEAD` contains the path
to a tag or branch.

If `HEAD` contains a direct reference or a symbolic reference to a tag (or a
remote branch), your repo is in what's known as "detached `HEAD`" state. What
this means is that the next `git commit` command will:

- create a new commit object.
- mutate the `.git/HEAD` file to point to the new commit's SHA as a direct ref.

This may not look bad, but contrast that to what the `git commit` command does
when `HEAD` is a symbolic ref to a local branch:

- create a new commits object.
- resolve HEAD to the branch file.
- mutate the branch file to point to the new commit.

Note that, in this case, `HEAD` is unchanged. Instead, git updates the branch
file, which is why branches give this illusion of continuity while working "on
a branch". This also means that you can switch to another commit or branch
_without losing your work_.

The other special refs are mostly for the internal workings of various git
commands, and we're not going to go into more details in this post.

## How this all fits together

In the simplest case, a git repo will have:

- `HEAD` as a symbolic ref to `refs/heads/master`.
- `refs/heads/master` as a direct ref to a current commit `C0`.
- `C0` pointing to a tree `T0`, and to a parent `C1`.
- `C1` pointing to a tree `T1`, and possibly more parents.
- `T0` may point to blobs `B0` and `B1` as well as a subfolder `T2`.
- `T1` may point to blobs `B0` and `B2` as well as a subfolder `T2`, assuming
  `B2` is the one file that was changed in-between `C0` and `C1`.

There are a few consequences of that approach. First, git does not store diffs.
Git computes diffs at the time of showing them. This means that the performance
of a git diff between two commits is a function of how different their content
is, not how far apart they are in the history DAG. Reusing trees makes for very
efficient diffing, as the algorithm knows it does not need to descend into
common trees at all.

Not storing diffs also means that any time you change a file, git stores a
completely new copy of that file. This may seem fairly wasteful in terms of
storage space, and to our first approximation it would be. Fortunately, there
is an easy solution to that.

## git packs

If you look into `.git/objects`, you'll see a bunch of two-letter folders, as
described above, but you'll also see two folders named `pack` and `info`.

Keeping all the objects as separate files can lead to a lot of filesystem
accesses. git was designed specifically for managing source code, which means
it generally expects lots of small files, which in turn means lots of file
accesses, which means lots of kernel calls, and that's slow.

So git can also store its objects in what is known as a "pack". The details are
not important here; what matters to our size concerns is that a pack is a
compressed representation, which means that for fairly transparent file formats
like plain text it's likely the compression algorithm will take care of not
repeating large chunks of identical text too many times.

You can explicitly ask git to "repack" all of its objects with the `git repack`
command.

## git garbage

In "normal" git operations, all objects should be saved forever. When you
create a new commit, it points to its parent, which means that you can't throw
away the parent. Creating new commits, including merge commits, is generally a
purely-additive operation.

Sometimes, though, some operations are destructive. The main user-level command
for that is `git rebase`, though `git reset` and `git commit --amend` can also
be used to create new commits that ignore part of the existing history.

More simply, you can just decide to throw away a branch after having made a few
commits that don't end up leading where you wanted. Or you can accidentally
work in a detached `HEAD` state, and then switch away to some other commit,
unintentionally creating unreachable commits (and potentially losing the
associated work).

When that happens, you can end up with "orphan" git objects, defined as objects
that cannot be reached from either HEAD or any of the user refs (branches and
tags). You can ask git to scan its object files (and packs) for such orphan
objects and remove them with the `git gc` command.

## Why git does not work with large binary files

By this point, you should be able to deduce why git repos would struggle with
large binary files: as every change is stored as a separate file, binary files
(which typically don't compress well even for apparently small changes) will
quickly take up quite a bit of space in either object or pack form.

But it's more than just storage. Because git is designed to work with text and
be quite fast, it _assumes_ it's dealing with small files, and some of its
algorithms reflect that. For example, the diff algorithm will (try to) load
both blobs in memory to compare them.

## Conclusion

You can use git every day for years without ever feeling like you need to know
any of this. But a lot of people express some level of frustration with git and
its various subcommands, and I hope that knowing this may help alleviate at
least some of that frustration.

For myself, I have found that knowing this has made working with git both more
pleasant and a lot safer, as I'm able to identify which commands might be
dangerous and how to properly use them without losing work.

[data]: /posts/2021-08-29-data-api
[sign]: https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work
