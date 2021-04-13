import java.util.function.Function;

public interface Maybe<A> {
    A get();
    <B> Maybe<B> bind(Function<A, Maybe<B>> f);
    static <A> Maybe<A> just(A a) { return new Just<>(a); }
    static <A> Maybe<A> none() { return new None<>(); }
    static class Just<A> implements Maybe<A> {
        private final A a;
        private Just(A a) {
            this.a = a;
        }
        public A get() { return a; }
        public <B> Maybe<B> bind(Function<A, Maybe<B>> f) {
            return f.apply(a);
        }
    }
    static class None<A> implements Maybe<A> {
        private None() {}
        public A get() { return null; }
        public <B> Maybe<B> bind(Function<A, Maybe<B>> f) {
            return new None<>();
        }
    }
}
