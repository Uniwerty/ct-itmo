package expression.exceptions;

import expression.BinaryOperation;
import expression.MainExpression;
import expression.TripleExpression;

public class CheckedMax extends CheckedBinaryOperation implements TripleExpression {
    public CheckedMax(TripleExpression first, TripleExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return Math.max(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }

    @Override
    protected String operator() {
        return " max ";
    }
}
