package expression.generic;

import java.util.Objects;

public class GenericVariable<T> implements GenericExpression<T> {
    private final String sign;

    public GenericVariable(String sign) {
        this.sign = sign;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return switch (sign) {
            case "x" -> x;
            case "y" -> y;
            case "z" -> z;
            default -> null;
        };
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof final GenericVariable<?> variable) {
            return Objects.equals(sign, variable.sign);
        }
        return false;
    }

    @Override
    public String toString() {
        return sign;
    }

    @Override
    public int hashCode() {
        return sign.hashCode();
    }
}
