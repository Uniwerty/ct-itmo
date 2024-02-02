package parser.lexic;

import parser.grammar.terminal.Token;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

public class Lexer {
    private final String string;
    private int position;
    private Token currentToken;

    public Lexer(String string) {
        this.string = string;
        position = 0;
    }

    public Token getCurrentToken() {
        return currentToken;
    }

    public void takeNextToken() throws ParseException {
        skipWhitespace();
        if (!hasNextCharacter()) {
            currentToken = Token.END;
        } else if (isSeparator()) {
            currentToken = Token.SEPARATOR;
        } else if (isLeftBracket()) {
            currentToken = Token.LEFT_BRACKET;
        } else if (isComma()) {
            currentToken = Token.COMMA;
        } else if (isRightBracket()) {
            currentToken = Token.RIGHT_BRACKET;
        } else if (isNullable()) {
            currentToken = Token.NULLABLE;
        } else {
            int start = position;
            if (isIdentifier()) {
                String name = string.substring(start, position);
                if (isVar(name)) {
                    currentToken = Token.VAR;
                } else if (isArray(name)) {
                    currentToken = Token.ARRAY;
                } else if (isMap(name)) {
                    currentToken = Token.MAP;
                } else {
                    currentToken = Token.NAME;
                }
            } else {
                throw new ParseException("Unknown token found", position);
            }
        }
    }

    public List<Token> getTokensList() throws ParseException {
        List<Token> tokens = new ArrayList<>();
        while (hasNextCharacter()) {
            takeNextToken();
            tokens.add(getCurrentToken());
            skipWhitespace();
        }
        return tokens;
    }

    public int getPosition() {
        return position;
    }

    private boolean isVar(String name) {
        return name.equals(Token.VAR.getValue());
    }

    private boolean isSeparator() {
        return isToken(Token.SEPARATOR);
    }

    private boolean isArray(String name) {
        return name.equals(Token.ARRAY.getValue());
    }

    private boolean isMap(String name) {
        return name.equals(Token.MAP.getValue());
    }

    private boolean isLeftBracket() {
        return isToken(Token.LEFT_BRACKET);
    }

    private boolean isComma() {
        return isToken(Token.COMMA);
    }

    private boolean isRightBracket() {
        return isToken(Token.RIGHT_BRACKET);
    }

    private boolean isNullable() {
        return isToken(Token.NULLABLE);
    }

    private boolean isToken(Token token) {
        String value = token.getValue();
        for (int i = 0; i < value.length(); i++) {
            if (isAvailablePosition(position + i) && string.charAt(position + i) != value.charAt(i)) {
                return false;
            }
        }
        position += value.length();
        return true;
    }

    private boolean isIdentifier() {
        int currentPosition = position;
        if (!Character.isJavaIdentifierStart(string.charAt(currentPosition))) {
            return false;
        }
        while (isAvailablePosition(currentPosition)
                && Character.isJavaIdentifierPart(string.charAt(currentPosition))) {
            currentPosition++;
        }
        position = currentPosition;
        return true;
    }

    private void skipWhitespace() {
        while (hasNextCharacter() && Character.isWhitespace(string.charAt(position))) {
            position++;
        }
    }

    private boolean hasNextCharacter() {
        return isAvailablePosition(position);
    }

    private boolean isAvailablePosition(int position) {
        return position < string.length();
    }
}
