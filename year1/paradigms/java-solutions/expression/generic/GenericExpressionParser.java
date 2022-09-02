package expression.generic;

import expression.exceptions.IllegalExpressionException;
import expression.parser.BaseParser;

public class GenericExpressionParser<T> {
    private final Type<T> type;

    public GenericExpressionParser(Type<T> type) {
        this.type = type;
    }

    public GenericExpression<T> parse(String string) throws IllegalExpressionException {
        ExpParse exp = new ExpParse(string);
        GenericExpression<T> expression = exp.parseMinMax();
        exp.charactersLeft();
        return expression;
    }

    private class ExpParse extends BaseParser {

        public ExpParse(String string) {
            super(string);
        }

        public GenericExpression<T> parseMinMax() throws IllegalExpressionException {
            GenericExpression<T> result = parseAddSub();
            skipWhitespace();
            while (test("min") || test("max")) {
                String operator = parseOperator();
                result = getExpression(result, parseAddSub(), operator);
            }
            return result;
        }

        public GenericExpression<T> parseAddSub() throws IllegalExpressionException {
            GenericExpression<T> result = parseMulDiv();
            skipWhitespace();
            while (test('+') || test('-')) {
                String operator = parseOperator();
                result = getExpression(result, parseMulDiv(), operator);
            }
            return result;
        }

        public GenericExpression<T> parseMulDiv() throws IllegalExpressionException {
            GenericExpression<T> result = parseOperand();
            skipWhitespace();
            while (test('*') || test('/')) {
                String operator = parseOperator();
                result = getExpression(result, parseOperand(), operator);
            }
            return result;
        }

        private GenericExpression<T> getExpression(GenericExpression<T> first, GenericExpression<T> second, String operator) {
            return switch (operator) {
                case "+" -> new GenericAdd<>(first, second, type);
                case "-" -> new GenericSubtract<>(first, second, type);
                case "*" -> new GenericMultiply<>(first, second, type);
                case "/" -> new GenericDivide<>(first, second, type);
                case "min" -> new GenericMin<>(first, second, type);
                case "max" -> new GenericMax<>(first, second, type);
                default -> null;
            };
        }

        private String parseOperator() throws IllegalExpressionException {
            if (take('m')) {
                if (take('i') && take('n')) {
                    if (checkWhitespace() || test('-') || test('(')) {
                        return "min";
                    }
                } else if (take('a') && take('x')) {
                    if (checkWhitespace() || test('-') || test('(')) {
                        return "max";
                    }
                }
            }
            if (test('+') || test('-') || test('*') || test('/')) {
                return "" + take();
            }
            throw new IllegalExpressionException(String.format("Illegal operator: %s", take()));
        }

        private GenericExpression<T> parseOperand() throws IllegalExpressionException {
            GenericExpression<T> result;
            skipWhitespace();
            int parity = 0;
            while (take('-')) {
                skipWhitespace();
                parity++;
            }
            boolean isConst = false;
            if (checkVariable()) {
                result = parseVariable();
            } else if (test('(')) {
                result = parseBrackets();
            } else if (checkDigit()) {
                result = parseConst(parity % 2 != 0);
                isConst = true;
            } else if (take('c') && take('o') && take('u') && take('n') && take('t')) {
                result = new GenericCount<>(parseOperand(), type);
            } else {
                throw new IllegalExpressionException(String.format("Illegal operand: %s", take()));
            }
            if (!isConst && parity % 2 != 0) {
                result = new GenericNegate<>(result, type);
            }
            if (test('m')) {
                back(1);
                if (!take(')') && !checkWhitespace()) {
                    throw new IllegalExpressionException(String.format("Illegal operand: %s", take()));
                }
            }
            skipWhitespace();
            return result;
        }

        private GenericExpression<T> parseConst(boolean isNegative) throws IllegalExpressionException {
            final StringBuilder number = new StringBuilder();
            if (isNegative) {
                number.append('-');
            }
            while (between('0', '9')) {
                number.append(take());
            }
            try {
                return new GenericConst<>(type.parseConst(number.toString()));
            } catch (NumberFormatException e) {
                throw new IllegalExpressionException(String.format("Illegal number: %s", number));
            }
        }

        private GenericExpression<T> parseVariable() {
            return new GenericVariable<>(String.valueOf(take()));
        }

        private GenericExpression<T> parseBrackets() throws IllegalExpressionException {
            if (!take('(')) {
                throw new IllegalExpressionException("( expected");
            }
            GenericExpression<T> result = parseMinMax();
            if (!take(')')) {
                throw new IllegalExpressionException(") expected");
            }
            return result;
        }

        private void charactersLeft() throws IllegalExpressionException {
            if (this.take() != END) {
                throw new IllegalExpressionException(String.format("Extra characters left: %s ...", this.take()));
            }
        }
    }
}
