package expression.parser;

import expression.MainExpression;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FunctionalInterface
public interface Parser {
    MainExpression parse(String expression);
}
