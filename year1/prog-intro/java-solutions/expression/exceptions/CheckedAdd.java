package expression.exceptions;

import expression.BinaryOperation;
import expression.Const;
import expression.MainExpression;
import expression.TripleExpression;

public class CheckedAdd extends CheckedBinaryOperation implements TripleExpression {
    public CheckedAdd(TripleExpression first, TripleExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x, int y, int z) throws OverflowException {
        int a = first.evaluate(x, y, z);
        int b = second.evaluate(x, y, z);
        int sum = a + b;
        if (a > 0 && b > 0 && sum <= 0 ||
        a < 0 && b < 0 && sum >= 0) {
            throw new OverflowException("overflow");
        }
        return sum;
    }

    @Override
    protected String operator() {
        return " + ";
    }
}
