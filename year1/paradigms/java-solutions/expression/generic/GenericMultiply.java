package expression.generic;

public class GenericMultiply<T> extends GenericBinaryOperation<T> implements GenericExpression<T> {
    public GenericMultiply(GenericExpression<T> first, GenericExpression<T> second, Type<T> type) {
        super(first, second, type);
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.multiply(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }

    @Override
    protected String operator() {
        return " * ";
    }
}
