package expression.generic;

public abstract class Type<T> {
    public abstract T add(T x, T y);

    public abstract T subtract(T x, T y);

    public abstract T multiply(T x, T y);

    public abstract T divide(T x, T y);

    public abstract T negate(T x);

    public abstract T min(T x, T y);

    public abstract T max(T x, T y);

    public abstract int count(T x);

    public abstract T parseConst(String number);

    public abstract T convertTo(int value);
}
