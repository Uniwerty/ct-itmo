package expression.generic;

public class GenericNegate<T> implements GenericExpression<T> {
    private final GenericExpression<T> expression;
    private final Type<T> type;

    public GenericNegate(GenericExpression<T> expression, Type<T> type) {
        this.expression = expression;
        this.type = type;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.negate(expression.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return "-(" + expression.toString() + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof final GenericNegate<?> negate) {
            return expression.equals(negate.expression);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return "-".hashCode() * 17 + expression.hashCode();
    }
}
