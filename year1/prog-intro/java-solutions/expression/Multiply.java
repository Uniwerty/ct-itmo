package expression;

public class Multiply extends BinaryOperation implements MainExpression {
    public Multiply(MainExpression first, MainExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x) {
        return first.evaluate(x) * second.evaluate(x);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return first.evaluate(x, y, z) * second.evaluate(x, y, z);
    }
    @Override
    protected String operator() {
        return " * ";
    }
}
