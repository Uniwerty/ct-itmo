package expression.generic;

import java.util.Objects;

public class GenericConst<T> implements GenericExpression<T> {
    private final T value;

    public GenericConst(T value) {
        this.value = value;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return this.value;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof final GenericConst<?> c) {
            return Objects.equals(value, c.value);
        }
        return false;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }
}
