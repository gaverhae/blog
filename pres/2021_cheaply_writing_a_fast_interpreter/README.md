# Cheaply writing a fast interpreter

A [talk] given by Neil Mitchell on various ways to improve the performance of a
naïve tree-walking intepreter and the tradeoffs between them.

[talk]: https://www.youtube.com/watch?v=V8dnIw3amLA

The talk focuses on "cheap" interpreters, defined essentially as not doing
runtime assembly generation.

> Note that things like generating ASM, smart code analysis/compile-time
> optimizations (inlining functions, realizing that 4 + 2 = 6, etc.) and adding
> "bigger" functions (like add3) is exlpicitly out of scope for this talk.
> Arguably, they are all orthogonal to the interpreter itself.

The talk uses the following running example to compare various approaches on
speed and complexity:

```rust
x = 100;
for (i = 1000; i != 0; i--) {
  x = x + 4 + x + 3;
  x = x + 2 + 4;
}
x
```

Uses + as the only function to maximize interpretive overhead. The starting
interpreter looks like:

```rust
fn f(x: @Expr, vs: V) -> i64 {
  match x {
    Lit(i) => *i,
    Var(u) => vs[u],
    Add(x, y) => f(x, vs) + f(y, vs),
    Assign(u, e) => vs[u] = f(e, vs),
    ...
```

where `x` is the AST and `V` is some form of variable storage. The talk does
not explicitly state whether the loop is part of the interpreted language, but
does mention "we've done the obvious things, like converting the x and i
variables to integers", so I think it's fair to assume the loop is indeed part
of the interpreted language.

We start with a performance difference of 570x. However, the Rust compiler has
rewritten the inner loop to
```rust
x = x + x + 13
```
i.e. one assignment and two additions instead of the original 5 additions and 2
assignments. So `+` is replaced with a function `add(x, y)` with a `noinline`
annotation to ensure we do indeed have 2 assignments and 5 function calls. With
that constraint, the "direct" Rust code only runs 6.4 times faster than the
tree-walking interpreter.


The first improvement is to turn the AST into closures, so we do not need to
keep walking down the AST for each iteration:

```rust
type K = Box<dyn Fn(V) -> i64>;

fn f(x: &Expr) -> K {
  match x {
    Lit(i) => {
      let i = *i;
      box move |_| i;
    }
    Add(x, y) => {
      let x = f(x);
      let y = f(y);
      box move |v| x(v) + y(v);
    }
    ...
```

So `f` is no longer an interpreter and has become a compiler, returning a
closure  `V -> i64` that then needs to be executed. The talk claims this is "a
third faster", which I guess means 4 times slower than the baseline.

This closure approach is coopting the host language stack. We may be able to do
better by managing our own VM, either stack- or register-based.

Neil mentions we start with a stack-based approach because "that's easier to
compile to", then just skips over how to compile the original expression to a
stack representation, and jumps to `i--`:

```
PUSH -1
GET $i
ADD
SET î
```

and the stack-based interpreter snippet:

```rust
loop {
  match tape.next() {
    PUSH => stack.push(tape.next());
    ADD => stack.push(
      stack.pop() + stack.pop()),
    ...
```

with the variables at the bottom of the stack so they can be accessed by index.
That ends up being slightly worse than the closure approach, because we jump
around too much in reading the tape. To avoid that, we basically do the same as
before: use closures instead of labels. So with closures on the tape instead of
labels we have to interpret, we can write functions like:

```rust
fn add(stack: Stack, tape: Tape) {
  stack.push(stack.pop() + stack.pop());
  let k = tape.next();
  k(stack, tape);
}
```

This is called "direct threading" because each function threads directly to the
next operation on the stack. This gets us about halfway to the baseline.

Finally, we can go for a register implementation, which the talk completely
skips over. I have no idea what that would look like, but Neil claims it's too
complicated in practice.

Summary:
- Naive AST walking is ~6.4 penalty.
- Closures are pretty cheap to implement and lower that to 4.8 penalty.
- Registers can go as low as 1.4 penalty, but are much harder to implement.

In the Starlark interpreter, these techniques yielded about half the benefits
that these "theoretical" penalties would suggest, which points to starlark
programs spending about half their time in interpretation (the other half being
spent evaluating builtins).
