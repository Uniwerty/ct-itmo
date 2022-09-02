package expression.generic;

public class GenericCount<T> implements GenericExpression<T> {
    private final GenericExpression<T> expression;
    private final Type<T> type;

    public GenericCount(GenericExpression<T> expression, Type<T> type) {
        this.expression = expression;
        this.type = type;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.convertTo(type.count(expression.evaluate(x, y, z)));
    }
    // toString
    // equals
    // hashCode
}
