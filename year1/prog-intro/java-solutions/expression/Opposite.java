package expression;

public class Opposite implements MainExpression {
    private MainExpression expression;

    public Opposite(MainExpression expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate(int x) {
        return -expression.evaluate(x);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return -expression.evaluate(x, y, z);
    }

    @Override
    public String toString() {
        return "-(" + expression.toString() + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Opposite) {
            final Opposite opposite = (Opposite) obj;
            return expression.equals(opposite.expression);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return "-".hashCode() * 17 + expression.hashCode();
    }
}
