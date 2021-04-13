public class Arith {

    public static Maybe<Double> add(Double a, Double b) {
        return Maybe.just(a + b);
    }

    public static Maybe<Double> sub(Double a, Double b) {
        return Maybe.just(a - b);
    }

    public static Maybe<Double> mul(Double a, Double b) {
        return Maybe.just(a * b);
    }

    public static Maybe<Double> div(Double a, Double b) {
        return b == 0.0 ? Maybe.none() : Maybe.just(a / b);
    }

    public static Maybe<Double> sqrt(Double a) {
        return a < 0.0 ? Maybe.none() : Maybe.just(Math.sqrt(a));
    }

    public static Maybe<Double> sqr_inv(Double a) {
        return Maybe.just(a)
            .bind(x -> sqrt(x))
            .bind(x -> div(1.0, x));
    }

    public static void main(String[] args) {
        Double d =
            Maybe.just(5.0)
            .bind(r -> add(r, 7.0))
            .bind(r -> sub(r, 2.0))
            .bind(r -> sqr_inv(r))
            .bind(r -> mul(r, 3.0))
            .get();
        System.out.println(d);
    }
}
