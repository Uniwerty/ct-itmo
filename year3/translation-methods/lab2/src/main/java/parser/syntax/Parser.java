package parser.syntax;

import parser.grammar.nonterminal.NonTerminal;
import parser.grammar.terminal.Token;
import parser.lexic.Lexer;
import parser.tree.Tree;

import java.text.ParseException;
import java.util.List;

public class Parser {
    private Lexer lexer;

    public Tree parse(String string) throws ParseException {
        lexer = new Lexer(string);
        lexer.takeNextToken();
        return parseS();
    }

    private Tree parseS() throws ParseException {
        Tree sTree = new Tree(NonTerminal.S.name())
                .addChild(getExpected(Token.VAR))
                .addChild(getExpected(Token.NAME))
                .addChild(getExpected(Token.SEPARATOR))
                .addChild(parseC());
        expect(Token.END);
        return sTree;
    }

    private Tree parseC() throws ParseException {
        Tree cTree = new Tree(NonTerminal.C.name());
        Token currentToken = lexer.getCurrentToken();
        if (currentToken == Token.ARRAY) {
            return cTree.addChild(parseA());
        } else if (currentToken == Token.MAP) {
            return cTree.addChild(parseM());
        }
        throw newParseException(List.of(Token.ARRAY, Token.MAP), currentToken);
    }

    private Tree parseA() throws ParseException {
        return new Tree(NonTerminal.A.name())
                .addChild(getExpected(Token.ARRAY))
                .addChild(getExpected(Token.LEFT_BRACKET))
                .addChild(parseT())
                .addChild(getExpected(Token.RIGHT_BRACKET))
                .addChild(parseN());
    }

    private Tree parseM() throws ParseException {
        return new Tree(NonTerminal.M.name())
                .addChild(getExpected(Token.MAP))
                .addChild(getExpected(Token.LEFT_BRACKET))
                .addChild(parseT())
                .addChild(getExpected(Token.COMMA))
                .addChild(parseT())
                .addChild(getExpected(Token.RIGHT_BRACKET))
                .addChild(parseN());
    }

    private Tree parseN() throws ParseException {
        Tree nTree = new Tree(NonTerminal.N.name());

        Token currentToken = lexer.getCurrentToken();
        if (currentToken == Token.NULLABLE) {
            nTree.addChild(new Tree(currentToken.getValue()));
            lexer.takeNextToken();
            return nTree;
        } else if (currentToken == Token.END
                || currentToken == Token.RIGHT_BRACKET
                || currentToken == Token.COMMA) {
            nTree.addChild(new Tree(Token.EMPTY.getValue()));
            return nTree;
        }
        throw newParseException(
                List.of(Token.NULLABLE, Token.END, Token.RIGHT_BRACKET, Token.COMMA),
                currentToken
        );
    }

    private Tree parseT() throws ParseException {
        Tree t = new Tree(NonTerminal.T.name());

        Token currentToken = lexer.getCurrentToken();
        if (currentToken == Token.NAME) {
            t.addChild(new Tree(currentToken.getValue()));
            lexer.takeNextToken();
            t.addChild(parseTPrime())
                    .addChild(parseN());
            return t;
        } else if (currentToken == Token.ARRAY || currentToken == Token.MAP) {
            return t.addChild(parseC());
        }
        throw newParseException(
                List.of(Token.NAME, Token.ARRAY, Token.MAP),
                currentToken
        );
    }

    private Tree parseTPrime() throws ParseException {
        Tree tPrime = new Tree(NonTerminal.T_PRIME.name());

        Token currentToken = lexer.getCurrentToken();
        if (currentToken == Token.LEFT_BRACKET) {
            tPrime.addChild(new Tree(currentToken.getValue()));
            lexer.takeNextToken();
            tPrime.addChild(parseT())
                    .addChild(getExpected(Token.RIGHT_BRACKET));
            return tPrime;
        } else if (currentToken == Token.NULLABLE
                || currentToken == Token.RIGHT_BRACKET
                || currentToken == Token.COMMA) {
            tPrime.addChild(new Tree(Token.EMPTY.getValue()));
            return tPrime;
        }
        throw newParseException(
                List.of(Token.LEFT_BRACKET, Token.NULLABLE, Token.RIGHT_BRACKET, Token.COMMA),
                currentToken
        );
    }

    private Tree getExpected(Token expected) throws ParseException {
        expect(expected);
        lexer.takeNextToken();
        return new Tree(expected.getValue());
    }

    private void expect(Token expected) throws ParseException {
        Token current = lexer.getCurrentToken();
        if (current != expected) {
            throw newParseException(List.of(expected), current);
        }
    }

    private ParseException newParseException(List<Token> expected, Token found) {
        String expectedOptions = String.join(
                " or ",
                expected.stream().map(Token::getValue).toList()
        );
        return new ParseException(
                "Expected " + expectedOptions + " but found " + found.getValue(),
                lexer.getPosition() - found.getValue().length()
        );
    }
}
