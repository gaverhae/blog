{:title "Graph search: Dijkstra, A*"
 :layout :post
 :tags ["haskell"]}

Over the past few weeks I've been taking part in [Advent of Code], which has
been a lot of fun. Graph search has come up a couple times, and it took me an
embarrassingly long time to remember the basics, so I'm writing this for my
future self. Both for future reference and in the hope that writing it down
will help me remember it all better.

### The problem

Loosely speaking, I'm talking about "find the shortest path" kind of problems:
you have a set of "nodes" and ways to move between them, with each movement
having a specific cost. Most nodes can be reached through multiple paths, and
in general the cost of a path depends on more than just its initial and final
nodes.

For the purpose of this blog post, I consider problems in which we have:

- Some representation of a "current state".
- A function which, given a state, returns a list of new states that can be
  reached in one "step", along with the cost of that step.
- A known initial state, `s0`, as well as at least one desired final state,
  which we'll model as a function that returns `true` on a final state. Most
  problems have only one such state, but there isn't much of a good reason to
  restrict ourselves at this point.

This is a pretty flexible definition. In particular, it doesn't require the
whole graph to "exist" before you navigate it. So it applies if you actually
have a graph to start with (say, trying to find the shortest path for a packet
to travel on a network of routers, or finding a series of flights to reach a
destination through various airports), or if you're moving on a grid, but also
to, say, finding the minimal number of moves to win a game of Solitaire.

### General approach

In abstract terms, the general approach is to:

- Keep track of where we are and how much it cost to get here.
- Compute all possible next states (by "moving" only one "step" from the
  current state), as well as the cost of reaching each of those new states.
- Add each of those states (with their cost) to the list of states we still
  need to explore.
- Avoid cycles, i.e. have a reliable way to know when we've already visited a
  state and avoid doing that again.
- Pick a state from the list and start over.

### Depth- and breadth-first

The name "depth-first" (just like "breadth-first") actually makes little sense
in a generalized graph context, but it's a useful analogy to tree traversals.
When traversing a tree, starting from the root and trying to visit all the
nodes up to and including leafs, it's easy to see how one can choose between
visiting either the children of the current node first, or its siblings.

Ignoring order for the time being, the code might look like this:

```clojure
(defn search
  [initial final? generate-moves]
  (let [INF Long/MAX_VALUE
        put-in (fn [m [k v]] (update m k (fnil min INF) v))]
    (loop [[state cost :as current] [initial 0]
           to-visit {}
           visited {}]
      (let [to-visit (if (<= (visited state INF) cost)
                       to-visit
                       (->> (generate-moves current)
                            (remove (fn [[nxt-state nxt-cost]]
                                      (>= nxt-cost (visited nxt-state INF))))
                            (reduce put-in
                                    to-visit)))
            visited (put-in visited current)]
        (if (empty? to-visit)
          (->> visited
               (filter (fn [[state cost]] (final? state)))
               (map (fn [[state cost]] cost))
               (reduce min))
          (recur (first to-visit)
                 (into {} (rest to-visit))
                 visited))))))
```

where we assume `generate-moves` takes in a tuple `[state, cost-so-far]` and
returns a list of `[state, total-cost]`.

The above code is essentially navigating through the possible paths at random.
To make it non-random, we need to switch the type of `to-visit` from a map to
something that preserves order. Specifically, we want a "last in first out"
behaviour for a depth-first search (so the "children" of the current node get
processed before the "children" of the "parent" node, a.k.a. "siblings"), which
is easily implemented with a list:


```clojure
(defn depth-first-search
  [initial final? generate-moves]
  (let [INF Long/MAX_VALUE
        put-in (fn [m [k v]] (update m k (fnil min INF) v))]
    (loop [[state cost :as current] [initial 0]
           to-visit () ;; changed to a list
           visited {}]
      (let [to-visit (if (<= (visited state INF) cost)
                       to-visit
                       (->> (generate-moves current)
                            (remove (fn [[nxt-state nxt-cost]]
                                      (>= nxt-cost (visited nxt-state INF))))
                            (reduce conj ;; changed `put-in` to `conj`
                                    to-visit)))
            visited (put-in visited current)]
        (if (empty? to-visit)
          (->> visited
               (filter (fn [[state cost]] (final? state)))
               (map (fn [[state cost]] cost))
               (reduce min))
          (recur (first to-visit)
                 (rest to-visit) ;; no need to rebuild a map anymore
                 visited))))))
```

The breadth-first approach is a little bit more tricky as we need a "first in
first out" behaviour and the common Clojure data structures don't support that
efficiently. There is, however, a little-known persistent queue data structure
in the Clojure standard library, that happens to not have any special syntax
for it. Here is how it could be used here:

```clojure
(defn breadth-first-search
  [initial final? generate-moves]
  (let [INF Long/MAX_VALUE
        put-in (fn [m [k v]] (update m k (fnil min INF) v))]
    (loop [[state cost :as current] [initial 0]
           to-visit clojure.lang.PersistentQueue/EMPTY ;; changed
           visited {}]
      (let [to-visit (if (<= (visited state INF) cost)
                       to-visit
                       (->> (generate-moves current)
                            (remove (fn [[nxt-state nxt-cost]]
                                      (>= nxt-cost (visited nxt-state INF))))
                            (reduce conj ;; changed `put-in` to `conj`
                                    to-visit)))
            visited (put-in visited current)]
        (if (empty? to-visit)
          (->> visited
               (filter (fn [[state cost]] (final? state)))
               (map (fn [[state cost]] cost))
               (reduce min))
          (recur (peek to-visit) ;; gets the "least recently added" item
                 (pop to-visit) ;; returns queue without (peek q)
                 visited))))))
```


If we assume that the cost of moving from one state to the next depends on both
states, neither breadth-first nor depth-first helps, though, because we have no
way to know, when looking at a node, whether we've already found the shortest
path to that node.

So whichever order of traversal we choose amongst random, breadth-first and
depth-first, we have to go through all possible paths to be confident we have
found the shortest one. That's not very efficient.[^1]

[^1]: Speaking of efficient, there is another problem with the depth-first and
breadth-first traversals as presented here: we will be visiting the same
nodes many, many times. Solving that is left as an exercise to the motivated
reader; they are presented here for illustration only, as a way to build up to
Dijkstra's and A\*; resolving this issue would lead to complications in the
code that are unrelated to the point of this post.

### Dijkstra

Enter [Dijkstra] and his algorithm. The key insight of Dijkstra's algorithm is
that the above approaches are neglecting a crucial piece of information: we
have a list of nodes _and the cost to reach each of them_. So instead of
deciding which node to expand next based on how we found the path to that node,
we can choose to expand the node with the smallest "cost to reach".

If we keep doing that at each iteration, we know for a fact that, on each
iteration, when we consider the "current" node, the cost we know for it is the
minimal possible cost to reach it (assuming all costs are positive). This means
that `visited` can become a simple set, rather than a map, but more importantly
it means that if the current node is a final one, _we can stop immediately_.

`to-visit` may need a bigger change, though. Walking through all possible paths
in order to find the minimum for each iteration would be very wasteful. The
approach generally suggested here is to keep the `to-visit` states in some form
of priority heap using the cost as a sorting key. In Clojure, we can achieve
that by using a [`java.util.PriorityQueue`] instance.

```clojure
(defn dijkstra-search
  [initial final? generate-moves]
  (let [to-visit (java.util.PriorityQueue. 100 compare)]
    (loop [[cost state] [0 initial]
           visited #{}]
      (when (not (visited state))
        (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
          (when (not (visited nxt-state))
            (.add to-visit [nxt-cost nxt-state]))))
      (if (final? state)
        cost
        (recur (.poll to-visit)
               (conj visited state))))))
```

By passing the `compare` function as the comparator we ensure that we'll get
Clojure's collection ordering, which for vectors is lexical order, i.e. states
will be ordered by cost first, then state itself. Note how we now work with
`[cost state]` tuples rather than `[state cost]` ones. (I have assumed that the
`generate-moves` function is unchanged for consistency with previous code
samples, but of course if you're only intrested in implementing Dijkstra you
could save a bit of vector construction by switching the order in
`generate-moves`.)

You may be bothered by the introduction of a mutable heap here. I don't
personally think it's a problem as the mutable state is completely encapsulated
in this case, but if you do want to avoid it, Clojure has a `sorted-map`
function that can be useful here. `sorted-map` is a function that returns a map
that keeps its keys in order, so we can use it in a similar way to the
PriorityQueue. The main difference is that we'll have to handle key conflicts
ourselves, which we can do by keeping, for each key, a list of states. This
means each iteration of our loop needs to handle a list of states, too.

```clojure
(defn dijkstra-search-imm
  [initial final? generate-moves]
  (loop [[cost states] [0 [initial]]
         to-visit (sorted-map 0 [initial])
         visited #{}]
    (let [to-visit (->> states
                        (remove visited)
                        (mapcat (fn [state] (generate-moves [state cost])))
                        (reduce (fn [m [s c]]
                                  (if (visited s)
                                    m
                                    (update m c (fnil conj #{}) s)))
                                (dissoc to-visit cost)))]
      (if (not (every? (complement final?) states))
        cost
        (recur (first to-visit)
               to-visit
               (reduce conj visited states))))))
```

In most cases, Dijkstra should run a lot faster than any of the previous
approaches. But we can go further.

### A\*

Just like Dijkstra observed that breadth-first approaches were not using the
cost information we have, Dijkstra's approach may not be using the goal
information we might have. In circumstances where we can have some measure of
how close we are to an end state, we can use that information to beat Dijkstra.

More formally, if we can, for each state, compute a number that
_**underestimates**_ the total possible cost to reach an end state from that
node, we can use A\*.  For example, if you are running down a maze and you know
the location of the exit, but not where all the walls are, a direct Euclidean
distance between your current position and the exit would be a good way to
underestimate the total distance you still have to travel.

The function used to generate this underestimated approximation is usually
called "the heuristic function", or just "the heuristic". The A\* algorithm is
a graph search in which we expand the nodes that have the minimal value for
(known cost to reach + heuristic), rather than just known cost to reach. In
effect, running A\* with a heuristic that always returns 0 devolves to
Dijkstra.

Assuming we have a heuristic, the code is very similar to Dijkstra's:

```clojure
(defn a-star-search
  [initial final? generate-moves heuristic]
  (let [to-visit (java.util.PriorityQueue. 100 compare)]
    (loop [[guess cost state] [(heuristic initial) 0 initial]
           visited #{}]
      (when (not (visited state))
        (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
          (when (not (visited nxt-state))
            (.add to-visit [(+ nxt-cost (heuristic nxt-state))
                            nxt-cost nxt-state]))))
      (if (final? state)
        cost
        (recur (.poll to-visit)
               (conj visited state))))))
```

The main point to note here is that, while we put the sum first so that's what
we sort on, we do keep track of the known cost, because we'll need it later on.

The immutable version using a `sorted-map` is left as an exercise to the
reader.

[Advent of Code]: https://adventofcode.com
[Dijkstra]: https://en.wikipedia.org/wiki/Edsger_W._Dijkstra
[`java.util.PriorityQueue`]: https://docs.oracle.com/javase/7/docs/api/java/util/PriorityQueue.html
