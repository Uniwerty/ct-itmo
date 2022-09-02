package expression.generic;

import expression.exceptions.IllegalExpressionException;

public class GenericTabulator implements Tabulator {
    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        Type<?> type = switch(mode) {
            case "i" -> new OverflowIntegerType();
            case "d" -> new DoubleType();
            case "bi" -> new BigIntegerType();
            case "u" -> new IntegerType();
            case "l" -> new LongType();
            case "s" -> new ShortType();
            default -> null;
        };
        return get(type, expression, x1, x2, y1, y2, z1, z2);
    }

    private <T> Object[][][] get(Type<T> type, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws IllegalExpressionException {
        Object[][][] table = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        GenericExpressionParser<T> parser = new GenericExpressionParser<>(type);
        GenericExpression<T> exp = parser.parse(expression);
        for (int i = 0; i <= x2 - x1; i++) {
            for (int j = 0; j <= y2 - y1; j++) {
                for (int k = 0; k <= z2 - z1; k++) {
                    try {
                        table[i][j][k] = exp.evaluate(type.convertTo(x1 + i), type.convertTo(y1 + j), type.convertTo(z1 + k));
                    } catch (Exception e) {
                        table[i][j][k] = null;
                    }
                }
            }
        }
        return table;
    }
}
