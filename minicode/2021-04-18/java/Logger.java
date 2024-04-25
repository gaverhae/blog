import java.util.function.Function;
import java.util.List;
import java.util.ArrayList;

public interface Logger<A> {
    <B> Logger<B> bind(Function<A, Logger<B>> f);
    static <A> Logger<A> pure(A a) {
        return new Impl.Wrapped<>(a);
    }

    A getValue();
    List<String> getLog();

    static class Impl {
        private Impl() {}
        private static class Wrapped<A> implements Logger<A> {
            public final A a;
            public final List<String> log;
            public Wrapped(A a) {
                this.a = a;
                this.log = new ArrayList<>();
                this.log.add(a.toString());
            }
            public Wrapped(A a, List<String> log) {
                this.a = a;
                this.log = log;
            }
            public <B> Logger<B> bind(Function<A, Logger<B>> f) {
                Logger<B> mb = f.apply(a);
                List<String> new_log = new ArrayList<>(log);
                new_log.addAll(mb.getLog());
                return new Wrapped<>(mb.getValue(), new_log);
            }
            public A getValue() {
                return a;
            }
            public List<String> getLog() {
                return new ArrayList<>(log);
            }
        }
    }

    static <A> Logger<A> _return(A a) {
        return pure(a);
    }

    public static void main(String[] args) {
        Logger<Integer> log1 = pure(1)
            .bind(a -> pure(a + 3))
            .bind(b -> pure(b + 4))
            .bind(c -> pure(c * 3));
        Logger<Integer> log2 = pure(1)
            .bind(a -> pure(a + 3)
            .bind(b -> pure(b + 4)
            .bind(c -> pure(c * 3))));
        Logger<Integer> log3 =
            pure(1)    .bind(a ->
            pure(a + 3).bind(b ->
            pure(b + 4).bind(c ->
            pure(c * 3))));
        Logger<Integer> log4 =
            pure    (    1)    .bind(a ->
            pure    (a + 3).bind(b ->
            pure    (b + 4).bind(c ->
            _return (c * b + a))));
        System.out.println(log1.getLog());
        System.out.println(log2.getLog());
        System.out.println(log3.getLog());
        System.out.println(log4.getLog());
    }
}
