package expression.exceptions;

import expression.MainExpression;
import expression.TripleExpression;

public class CheckedNegate implements TripleExpression {
    private TripleExpression expression;

    public CheckedNegate(TripleExpression expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int a = expression.evaluate(x, y, z);
        int negate = -a;
        if (a > 0 && negate > 0 || a < 0 && negate < 0) {
            throw new OverflowException("overflow");
        }
        return negate;
    }

    @Override
    public String toString() {
        return "-(" + expression.toString() + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof CheckedNegate) {
            final CheckedNegate negate = (CheckedNegate) obj;
            return expression.equals(negate.expression);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return "-".hashCode() * 17 + expression.hashCode();
    }
}
