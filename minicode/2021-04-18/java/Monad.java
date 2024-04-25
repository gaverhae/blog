import java.util.function.Function;

public interface Monad<A> {
    <B> Monad<B> bind(Monad<A> ma, Function<A, Monad<B>> f);
}
