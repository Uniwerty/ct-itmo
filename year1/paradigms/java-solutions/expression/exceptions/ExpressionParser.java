package expression.exceptions;

import expression.*;
import expression.parser.BaseParser;

public class ExpressionParser implements Parser {

    @Override
    public TripleExpression parse(String string) throws IllegalExpressionException {
        ExpParse exp = new ExpParse(string);
        TripleExpression expression = exp.parseMinMax();
        exp.charactersLeft();
        return expression;
    }

    private class ExpParse extends BaseParser {

        public ExpParse(String string) {
            super(string);
        }

        public TripleExpression parseMinMax() throws IllegalExpressionException {
            TripleExpression result = parseAddSub();
            skipWhitespace();
            while (test("min") || test("max")) {
                String operator = parseOperator();
                result = getExpression(result, parseAddSub(), operator);
            }
            return result;
        }

        public TripleExpression parseAddSub() throws IllegalExpressionException {
            TripleExpression result = parseMulDiv();
            skipWhitespace();
            while (test('+') || test('-')) {
                String operator = parseOperator();
                result = getExpression(result, parseMulDiv(), operator);
            }
            return result;
        }

        public TripleExpression parseMulDiv() throws IllegalExpressionException {
            TripleExpression result = parseOperand();
            skipWhitespace();
            while (test('*') || test('/')) {
                String operator = parseOperator();
                result = getExpression(result, parseOperand(), operator);
            }
            return result;
        }

        private TripleExpression getExpression(TripleExpression first, TripleExpression second, String operator) {
            return switch (operator) {
                case "+" -> new CheckedAdd(first, second);
                case "-" -> new CheckedSubtract(first, second);
                case "*" -> new CheckedMultiply(first, second);
                case "/" -> new CheckedDivide(first, second);
                case "min" -> new CheckedMin(first, second);
                case "max" -> new CheckedMax(first, second);
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

        private TripleExpression parseOperand() throws IllegalExpressionException {
            TripleExpression result = null;
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
                        result = new CheckedNegate(result);
                    }
                } else if (test('(')) {
                    result = parseBrackets();
                    if (parity % 2 == 1) {
                        result = new CheckedNegate(result);
                    }
                } else if (checkDigit()) {
                    result = parseConst(true);
                    parity--;
                    if (parity % 2 == 1) {
                        result = new CheckedNegate(result);
                    }
                } else if (take('t') && take('0') && (checkWhitespace() || test('-') || test('('))) {
                    result = new CheckedLowerBits(parseOperand());
                    if (parity % 2 == 1) {
                        result = new CheckedNegate(result);
                    }
                } else if (take('l') && take('0') && (checkWhitespace() || test('-') || test('('))) {
                    result = new CheckedUpperBits(parseOperand());
                    if (parity % 2 == 1) {
                        result = new CheckedNegate(result);
                    }
                } else {
                    throw new IllegalExpressionException(String.format("Illegal operand: %s", take()));
                }
            } else if (checkVariable()) {
                result = parseVariable();
            } else if (test('(')) {
                result = parseBrackets();
            } else if (checkDigit()) {
                result = parseConst(false);
            } else if (take('t') && take('0') && (checkWhitespace() || test('-') || test('('))) {
                result = new CheckedLowerBits(parseOperand());
            } else if (take('l') && take('0') && (checkWhitespace() || test('-') || test('('))) {
                result = new CheckedUpperBits(parseOperand());
            } else {
                throw new IllegalExpressionException(String.format("Illegal operand: %s", take()));
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

        private TripleExpression parseConst(boolean isNegative) throws IllegalExpressionException {
            final StringBuilder number = new StringBuilder();
            if (isNegative) {
                number.append('-');
            }
            while (between('0', '9')) {
                number.append(take());
            }
            try {
                return new Const(Integer.parseInt(number.toString()));
            } catch (NumberFormatException e) {
                throw new IllegalExpressionException(String.format("Illegal number: %s", number.toString()));
            }
        }

        private TripleExpression parseVariable() {
            return new Variable(String.valueOf(take()));
        }

        private TripleExpression parseBrackets() throws IllegalExpressionException {
            if (!take('(')) {
                throw new IllegalExpressionException("( expected");
            }
            TripleExpression result = parseMinMax();
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
