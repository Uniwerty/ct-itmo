package expression.exceptions;

import expression.TripleExpression;

public class CheckedUpperBits implements TripleExpression {
    private TripleExpression expression;

    public CheckedUpperBits(TripleExpression expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int a = expression.evaluate(x, y, z);
        for (int i = 0; i <= 32; i++) {
            if (a >>> i == 0) {
                return 32 - i;
            }
        }
        return 0;
    }

    @Override
    public String toString() {
        return "l0(" + expression.toString() + ")";
    }
}
