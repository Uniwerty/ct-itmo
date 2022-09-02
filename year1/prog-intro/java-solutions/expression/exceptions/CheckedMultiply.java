package expression.exceptions;

import expression.BinaryOperation;
import expression.MainExpression;
import expression.TripleExpression;

public class CheckedMultiply extends CheckedBinaryOperation implements TripleExpression {
    public CheckedMultiply(TripleExpression first, TripleExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int a = first.evaluate(x, y, z);
        int b = second.evaluate(x, y, z);
        int mul = a * b;
        if (a > 0 && b > 0 && mul <= 0 || a < 0 && b < 0 && mul <= 0 ||
        a < 0 && b > 0 && mul >= 0 || a > 0 && b < 0 && mul >= 0 ||
        a != 0 && mul / a != b) {
            throw new OverflowException("overflow");
        }
        return mul;
    }

    @Override
    protected String operator() {
        return " * ";
    }
}
