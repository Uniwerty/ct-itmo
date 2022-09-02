package expression.generic;

public class GenericSubtract<T> extends GenericBinaryOperation<T> implements GenericExpression<T> {
    public GenericSubtract(GenericExpression<T> first, GenericExpression<T> second, Type<T> type) {
        super(first, second, type);
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.subtract(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }

    @Override
    protected String operator() {
        return " - ";
    }
}