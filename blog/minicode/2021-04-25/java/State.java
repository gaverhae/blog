import java.util.function.Function;

public final class State<S, A> {
    public static final class P<S, A> {
        public final S s;
        public final A a;
        private P(S s, A a) {
            this.s = s;
            this.a = a;
        }
        public String toString() {
            return "P[" + s + ", " + a + "]";
        }
    }

    private final Function<S, P<S, A>> run_state;

    private State(Function<S, P<S, A>> run_state) {
        this.run_state = run_state;
    }

    public <B> State<S, B> bind(Function<A, State<S, B>> f) {
        return new State<>(s0 -> {
            P<S, A> p = this.run_state.apply(s0);
            return f.apply(p.a).run_state.apply(p.s);
        });
    }

    public static <S, A> State<S, A> pure(A a) {
        return new State<>(s -> new P<>(s, a));
    }

    public static <S> State<S, S> get() {
        return new State<>(s -> new P<>(s, s));
    }

    public static <S> State<S, Void> put(S new_state) {
        return new State<>(_s -> new P<>(new_state, null));
    }

    public static <S, A> P<S, A> run(State<S, A> m, S init) {
        return m.run_state.apply(init);
    }

    // --

    public static final class Stack {
        private final Integer head;
        private final Stack tail;
        public Stack(Integer h, Stack t) {
            this.head = h;
            this.tail = t;
        }
        public String toString() {
            Stack s = this.tail;
            StringBuilder sb = new StringBuilder();
            sb.append("Stack(" + this.head);
            while (s != null) {
                sb.append(", " + s.head);
                s = s.tail;
            }
            sb.append(")");
            return sb.toString();
        }
    }


    public static State<Stack, Void> push(Integer a) {
        return State.<Stack>get().bind(old_stack ->
               put(new Stack(a, old_stack)));
    }

    public static State<Stack, Integer> pop() {
        return State.<Stack>get().bind(old_stack ->
               put(old_stack.tail).bind(_1 ->
               pure(old_stack.head)));
    }

    public static void main(String[] args) {
        State<Stack, Void> stack_computation =
            pop()      .bind(a ->
            pop()      .bind(b ->
            push(a + b)));

        Stack init_1 = new Stack(10, new Stack(20, null));
        Stack init_2 = new Stack(13, new Stack(256, null));

        System.out.println(run(stack_computation, init_1));
        System.out.println(run(stack_computation, init_2));
    }
}
