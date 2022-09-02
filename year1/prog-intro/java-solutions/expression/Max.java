package expression;

public class Max extends BinaryOperation implements MainExpression {
    public Max(MainExpression first, MainExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x) {
        return Math.max(first.evaluate(x), second.evaluate(x));
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
