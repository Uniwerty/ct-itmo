package expression.generic;

import java.util.Objects;

public abstract class GenericBinaryOperation<T> implements GenericExpression<T> {
    protected final GenericExpression<T> first;
    protected final GenericExpression<T> second;
    protected final Type<T> type;

    public GenericBinaryOperation(GenericExpression<T> first, GenericExpression<T> second, Type<T> type) {
        this.first = first;
        this.second = second;
        this.type = type;
    }

    // :NOTE:/2 evaluation of arguments should be extracted in the abstract class
    @Override
    public abstract T evaluate(T x, T y, T z);

    protected abstract String operator();

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof final GenericBinaryOperation<?> binaryOperation) {
            return Objects.equals(operator(), binaryOperation.operator()) &&
                    Objects.equals(first, binaryOperation.first) &&
                    Objects.equals(second, binaryOperation.second);
        }
        return false;
    }

    @Override
    public String toString(){
        String string = "(" +
                first.toString() +
                operator() +
                second.toString() +
                ")";
        return string;
    }

    @Override
    public int hashCode() {
        // :NOTE:/2 Objects.hash(operator, first, second)
        return ((first.hashCode() * 17) + second.hashCode()) * 17 + operator().hashCode();
    }
}
