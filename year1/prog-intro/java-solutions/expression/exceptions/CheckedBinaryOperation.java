package expression.exceptions;

import expression.MainExpression;
import expression.TripleExpression;

import java.util.Objects;

public abstract class CheckedBinaryOperation implements TripleExpression {
    protected final TripleExpression first;
    protected final TripleExpression second;

    public CheckedBinaryOperation(TripleExpression first, TripleExpression second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public abstract int evaluate(int x, int y, int z);

    protected abstract String operator();

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof CheckedBinaryOperation) {
            final CheckedBinaryOperation binaryOperation = (CheckedBinaryOperation) obj;
            return Objects.equals(operator(), binaryOperation.operator()) &&
                    Objects.equals(first, binaryOperation.first) &&
                    Objects.equals(second, binaryOperation.second);
        }
        return false;
    }

    @Override
    public String toString(){
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("(");
        stringBuilder.append(first.toString());
        stringBuilder.append(operator());
        stringBuilder.append(second.toString());
        stringBuilder.append(")");
        return stringBuilder.toString();
    }

    @Override
    public int hashCode() {
        return ((first.hashCode() * 17) + second.hashCode()) * 17 + operator().hashCode();
    }
}
