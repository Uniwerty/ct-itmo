package expression;

import java.util.Objects;

public class Variable implements MainExpression {
    private final String sign;

    public Variable(String sign) {
        this.sign = sign;
    }

    @Override
    public int evaluate(int x) {
        return x;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return switch (sign) {
            case "x" -> x;
            case "y" -> y;
            case "z" -> z;
            default -> 0;
        };
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Variable) {
            final Variable variable  = (Variable) obj;
            return Objects.equals(sign, variable.sign);
        }
        return false;
    }

    @Override
    public String toString() {
        return sign;
    }

    @Override
    public int hashCode() {
        return sign.hashCode();
    }
}
