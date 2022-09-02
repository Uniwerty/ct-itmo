package expression.exceptions;

import expression.BinaryOperation;
import expression.MainExpression;
import expression.TripleExpression;

public class CheckedSubtract extends CheckedBinaryOperation implements TripleExpression {
    public CheckedSubtract(TripleExpression first, TripleExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int a = first.evaluate(x, y, z);
        int b = second.evaluate(x, y, z);
        int sub = a - b;
        if (a > b && sub < 0 || a < b && sub > 0) {
            throw new OverflowException("overflow");
        }
        return sub;
    }

    @Override
    protected String operator() {
        return " - ";
    }
}