package expression.parser;

import expression.*;

public class ExpressionParser implements Parser {
    @Override
    public MainExpression parse(String expression) {
        return new ExpParse(expression).parseMinMax();
    }

    private class ExpParse extends BaseParser {
        public ExpParse(String string) {
            super(string);
        }

        public MainExpression parseMinMax() {
            MainExpression result = parseAddSub();
            while (test("min") || test("max")) {
                String operator = parseOperator();
                result = getExpression(result, parseAddSub(), operator);
            }
            return result;
        }

        public MainExpression parseAddSub() {
            MainExpression result = parseMulDiv();
            while (test('+') || test('-')) {
                String operator = parseOperator();
                result = getExpression(result, parseMulDiv(), operator);
            }
            return result;
        }

        public MainExpression parseMulDiv() {
            MainExpression result = parseOperand();
            while (test('*') || test('/')) {
                String operator = parseOperator();
                result = getExpression(result, parseOperand(), operator);
            }
            return result;
        }

        private MainExpression getExpression(MainExpression first, MainExpression second, String operator) {
            return switch (operator) {
                case "+" -> new Add(first, second);
                case "-" -> new Subtract(first, second);
                case "*" -> new Multiply(first, second);
                case "/" -> new Divide(first, second);
                case "min" -> new Min(first, second);
                case "max" -> new Max(first, second);
                default -> null;
            };
        }

        private String parseOperator() {
            if (test('+') || test('-') || test('*') || test('/')) {
                return "" + take();
            } else if (take('m')) {
                if (take('i') && take('n')) {
                    return "min";
                }
                if (take('a') && take('x')) {
                    return "max";
                }
            }
            return null;
        }

        private MainExpression parseOperand() {
            MainExpression result = null;
            skipWhitespace();
            if (take('-')) {
                int parity = 1;
                skipWhitespace();
                while (take('-')) {
                    skipWhitespace();
                    parity++;
                }
                if (checkVariable()) {
                    result = parseVariable();
                    if (parity % 2 == 1) {
                        result = new Opposite(result);
                    }
                } else if (test('(')) {
                    result = parseBrackets();
                    if (parity % 2 == 1) {
                        result = new Opposite(result);
                    }
                } else if (checkDigit()) {
                    result = parseConst(true);
                    parity--;
                    if (parity % 2 == 1) {
                        result = new Opposite(result);
                    }
                }
            } else if (test('(')) {
                result = parseBrackets();;
            } else if (checkVariable()) {
                result = parseVariable();
            } else if (checkDigit()) {
                result = parseConst(false);
            }
            skipWhitespace();
            return result;
        }

        private MainExpression parseConst(boolean isNegative) {
            final StringBuilder number = new StringBuilder();
            if (isNegative) {
                number.append('-');
            }
            while (between('0', '9')) {
                number.append(take());
            }
            return new Const(Integer.parseInt(number.toString()));
        }

        private MainExpression parseVariable() {
            return new Variable(String.valueOf(take()));
        }

        private MainExpression parseBrackets() {
            take('(');
            MainExpression result = parseMinMax();
            take(')');
            return result;
        }
    }
}
