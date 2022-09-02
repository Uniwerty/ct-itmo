package expression.generic;

public class GenericMax<T> extends GenericBinaryOperation<T> implements GenericExpression<T> {
    public GenericMax(GenericExpression<T> first, GenericExpression<T> second, Type<T> type) {
        super(first, second, type);
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.max(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }

    @Override
    protected String operator() {
        return " max ";
    }
}
