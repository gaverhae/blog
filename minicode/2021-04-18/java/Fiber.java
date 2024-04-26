import java.util.function.Function;
import java.util.function.Supplier;
import java.util.List;
import java.util.LinkedList;
import java.util.Queue;

public interface Fiber<A> {
    <B> Fiber<B> bind(Function<A, Fiber<B>> f);
    <B> B ifReturn(Function<A, B> f, Supplier<B> s);
    Fiber<A> step();

    static class Impl {
        private Impl() {}
        private final static class Return<A> implements Fiber<A> {
            public final A a;
            public Return(A a) {
                this.a = a;
            }
            public <B> Fiber<B> bind(Function<A, Fiber<B>> f) {
                return new Bind<A, B>(this, f);
            }
            public <B> B ifReturn(
                    Function<A, B> f,
                    Supplier<B> s) {
                return f.apply(a);
            }
            public Fiber<A> step() {
                return this;
            }
        }
        private final static class Bind<A, B> implements Fiber<B> {
            public final Fiber<A> ma;
            public final Function<A, Fiber<B>> f;
            public Bind(Fiber<A> ma, Function<A, Fiber<B>> f) {
                this.ma = ma;
                this.f = f;
            }
            public <C> Fiber<C> bind(Function<B, Fiber<C>> f) {
                return new Bind<>(this, f);
            }
            public <C> C ifReturn(
                    Function<B, C> f,
                    Supplier<C> s) {
                return s.get();
            }
            public Fiber<B> step() {
                return ma.ifReturn(f, () -> new Bind<A, B>(ma.step(), f));
            }
        }
    }

    static <A> Fiber<A> pure(A a) {
        return new Impl.Return<A>(a);
    }

    static <A> List<A> exec(List<Fiber<A>> ls) {
        List<A> rets = new LinkedList<>();
        Queue<Fiber<A>> q = new LinkedList<>(ls);
        while(!q.isEmpty()) {
            Fiber<A> f = q.remove();
            f.ifReturn(
                a -> rets.add(a),
                () -> q.add(f.step()));
        }
        return rets;
    }

    static <A> Fiber<A> _return(A a) {
        return pure(a);
    }

    static Fiber<Void> debug(String msg) {
        System.out.println(msg);
        return pure(null);
    }


    public static void main(String[] args) {
        Fiber<Double> f1 =
            pure          (5) .bind(a ->
            debug("thread 1") .bind(   _1 ->
            pure      (a + 3) .bind(b -> 
            debug("thread 1") .bind(   _2 ->
            pure      (b + 5) .bind(c ->
            debug("thread 1") .bind(   _3 ->
            pure      (b * c) .bind(i ->
            debug("thread 1") .bind(   _4 ->
            _return (1.0 * i)
            ))))))));
        Fiber<Double> f2 =
            pure                (1) .bind(a ->
            debug("thread 2") .bind(    _1 ->
            pure                (2) .bind(b ->
            debug("thread 2") .bind(    _2 ->
            pure                (1) .bind(c ->
            debug("thread 2") .bind(    _3 ->
            pure            (b * b) .bind(b2 ->
            debug("thread 2") .bind(    _4 ->
            pure            (a * c) .bind(ac ->
            debug("thread 2") .bind(    _5 ->
            pure      (b2 - 4 * ac) .bind(delta ->
            debug("thread 2") .bind(    _6 ->
            pure (Math.sqrt(delta)) .bind(rac ->
            debug("thread 2") .bind(    _7 ->
            _return ((-b + rac) / (2 * a))
            ))))))))))))));

        System.out.println(exec(new LinkedList<Fiber<Double>>() {{ add(f1); add(f2); }}));
        System.out.println(exec(new LinkedList<Fiber<Double>>() {{ add(f2); add(f1); }}));
    }
}
