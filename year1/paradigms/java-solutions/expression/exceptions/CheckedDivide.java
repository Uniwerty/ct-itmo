package expression.exceptions;

import expression.BinaryOperation;
import expression.MainExpression;
import expression.TripleExpression;

public class CheckedDivide extends CheckedBinaryOperation implements TripleExpression {
    public CheckedDivide(TripleExpression first, TripleExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int a = first.evaluate(x, y, z);
        int b = second.evaluate(x, y, z);
        if (b == 0) {
            throw new DivisionByZeroException("division by zero");
        }
        int div = a / b;
        if (a > 0 && b > 0 && div < 0 || a < 0 && b < 0 && div < 0 ||
                a < 0 && b > 0 && div > 0 || a > 0 && b < 0 && div > 0) {
            throw new OverflowException("overflow");
        }
        return div;
    }

    @Override
    protected String operator() {
        return " / ";
    }
}
