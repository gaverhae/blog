{:title "Tools You Should Know About: tmux"
 :layout :post
 :tags ["tyska"]}

## In a nutshell

From the official wiki:

> [tmux] is a terminal multiplexer. It lets you switch easily between several
> programs in one terminal, detach them (they keep running in the background)
> and reattach them to a different terminal.

Alternatively, you can think of it as a tiling window manager for your
terminal: it lets you split your terminal in "windows", which tmux calls
"panes", and even simulate multiple "screens", which tmux calls "windows".

## Why you should know about it

I use tmux all the time. I hardly ever have a terminal open _without_ running
tmux inside it. In fact, I'm typing this text in a Vim instance running in
tmux:

![tmux](/img/blog/2021/tmux.png)

Even if I don't particularly plan to use any tmux feature, I still start a tmux
session, because sometimes splitting a pane is useful even if I hadn't planned
for it.

So I'd really recommend using it all the time, once you've learned it. And
there really isn't that much to learn to get huge mileage out of it.

## Feature highlights

tmux can do a lot of things; there's [a whole book][book] about it. I am
obviously not going to try and replicate a whole book in a single blog post,
but I will provide a few highlights of ways in which tmux has been tremendously
useful for me.

### Panes

tmux gives me the ability to divide a single OS window into multiple panes.
It's pretty rare that I'd only want a single terminal, so this comes in handy.
It may seem that the same can be achieved by simply opening multiple terminal
windows, but then I'd have to manage those windows. I'd also need to learn my
OS-specific key bindings to switch between windows, and remember the
differences when I switch OS (I switch between macOS and Linux on a regular
basis).

Or perhaps I'd even need to - gasp - _use the mouse_ to switch between
terminals. Reaching for my mouse is quite frankly one of the very last things
I'd want to do when I'm working in a terminal. It's just not where my hands
are.

Having multiple (tmux) windows is also very useful in that it lets me maintain
multiple contexts easily. For example, if I'm working on a Clojure project, I
like having one pane with Vim, one pane with a running REPL, and one pane with
nothing specific (i.e. just running a shell) for the occasional odd command. I
can set these three panes up to be layed out how I like them in my terminal
window, and I don't have to mess with that layout if I suddenly need to work on
something unrelated.

### Sessions

When you run tmux, it starts a server in the background, which starts a
session, and then you connect to it. This means that you can detach your
terminal from the session, and leave the session running in the background.

This can occasionally be useful locally. For example, even if I have twelve
terminals open (which is not all that uncommon), I can immediately install
iTerm2 upgrades without having to close all of them and lose my context. I can
just detach my (usually just one) tmux session, close the terminal window, let
iTerm2 do its upgrade process, and then simply reattach my tmux session in a
new iTerm2 window.

Where sessions really shine, though, is when dealing with remote servers.
Beyond the obvious ability to easily have multiple terminals open on a remote
server through a single connection, the ability to detach sessions gives you
two very convenient features.

First, it allows your context and running programs to survive a connection
interruption. Your VPN crashes in the middle of editing a file? No problem.
Just reconnect, reattach, and your file will still be open in your text editor.
Your had started a somewhat long running compression process? Still running.
You don't want to _wait_ for a command to complete, because you have to
(physically) move to a new location, and won't have connectivity on the way?
You can start the long-running command in a tmux session, close your
connection, and come back to it a few hours later when you have a convenient
connection again.

Second, you can use this to run simple servers with very little ceremony. Don't
remember how to combine `nohup` with subshells to get a truly background
daemon? No problem: just start your server process in the foreground in a tmux
session. It will keep running if your connection breaks, and you can easily see
the outputs right there in your terminal. In fact, I have one such server
running right now: a simple `python -m http.server` command to serve a preview
of a documentation change, running on an ephemeral GCS instance within my
company VPN. Setting up a "real" server for that would be too much hassle, but
just running one command gets the bar low enough that people reviewing my PR
can look at the rendered HTML instead of having to infer it from an RST diff on
GitHub.

### Synchronized panes

This may be a somewhat more specialized use-case, but it occasionally happens
to me that I want to apply the same set of operations to multiple servers, or
do the same set of operations in multiple folders.

In general, if you need ten servers with the same configuration, there are
specialized deployment tools for that (puppet, terraform, Docker, ansible,
etc.). But sometimes those are a bit overkill, and all you really want to do is
run one or two Bash commands. But on ten machines.

tmux has a feature it calls "synchornized panes", which synchronizes the input
of all the panes in a given window. I've used this in the past to set up 16
Hadoop servers, for example, where I used a script (yes, tmux is also
scriptable!) to open up 16 panes on the same window, and then was able to run
the handful of Docker commands needed on each machine independently, in
parallel.

A few months ago I used this to run an integration test on ten machines with
ten sets of parameters. The last step (starting the test, specifying the
parameters) was different on each machine, but setting up the machine,
downloading the code, compiling it, etc. were all common, and I was very glad I
did not have to do them ten times. A few days ago I used this to install a new
version of a guest VM on five different servers. Sometimes I'll use it just to
tail logs on many servers at the same time, so I can keep an eye on all of
them.

It's not a feature I use _very_ often, but when I do use it I'm very glad I
have it.

## My config file

Like many unix tools, tmux has a high degree of configurability. Here is my
configuration file for it (`~/.tmux.conf`):

```plaintext
# Change prefix key to Ctrl+a
unbind C-b
set -g prefix C-a
bind a send-keys C-a

# Last active window
unbind l
bind C-a last-window

# Copy mode
bind Escape copy-mode
# Use Vi mode
setw -g mode-keys vi

# More straightforward key bindings for splitting
bind | split-window -h -c '#{pane_current_path}'
bind - split-window -v -c '#{pane_current_path}'

# History
set -g history-limit 1000000

# Pane
unbind o
bind C-s select-pane -t:.+

# Terminal emulator window title
set -g set-titles on
set -g set-titles-string '#S:#I.#P #W'

# Status Bar
set -g status-bg black
set -g status-fg white
set -g status-interval 1
set -g status-left '#[fg=green]#H#[default]'
set -g status-right '#[default] #[fg=cyan,bold]%Y-%m-%d %H:%M:%S#[default]'

# Notifying if other windows has activities
setw -g monitor-activity on
set -g visual-activity on

# Highlighting the active window in status bar
setw -g window-status-current-style bg=red

# Highlighting the current pane
set -g pane-border-style bg=black,fg=green
set -g pane-active-border-style bg=blue,fg=green

# Clock
setw -g clock-mode-colour green
setw -g clock-mode-style 24

# Setting TERM variable
set -g default-terminal "screen-256color"
```

The most important commands I use regularly are:

- `CTRL-A -`: split current pane horizontally.
- `CTRL-A |`: split current pane vertically.
- `CTRL-A CTRL-S`: select next pane in current window.
- `CTRL-A c`: create a new window.
- `CTRL-A n`: move to next window.
- `CTRL-A p`: move to previous window.
- `CTRL-A CTRL-A`: flip back and forth between last two visited windows.
- `CTRL-A <number>`: jump to window `<number>`.
- `CTRL-A :`: open up the tmux command prompt, which I mostly use to toggle
  `sunchronize-panes` (`:set synchronize-panes on`/`off`.
- `CTRL-A ESC`: enter "copy" mode, which lets me navigate the scrolbuffer.
  Space starts the selection process, enter copies the selection into tmux's
  clipboard.
- `CTRL-A ]`: paste (from tmux's clipboard).

If you're just starting, I'd recommend focusing on the first three.  There's
already quite a bit of value in those.

## Conclusion

That's all for today. I really hope you give tmux a try; it's made my work so
much more conveninent. I'm tempted to say I coudln't imagine working without
it, but I can, and it'd suck.

[tmux]: https://github.com/tmux/tmux/wiki
[book]: https://www.amazon.com/tmux-2-Productive-Mouse-Free-Development/dp/1680502212
