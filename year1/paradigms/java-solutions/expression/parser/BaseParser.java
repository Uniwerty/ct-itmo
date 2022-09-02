package expression.parser;

public class BaseParser {
    public static final char END = 0;
    private final String string;
    private char ch;
    private int pos;

    public BaseParser(String string) {
        this.string = string;
        ch = string.charAt(0);
    }

    protected boolean test(final char expected) {
        return ch == expected;
    }

    protected boolean test(final String expected) {
        int index = pos;
        for (int i = 0; i < expected.length(); i++) {
            if (string.charAt(index) != expected.charAt(i)) {
                return false;
            }
            index++;
        }
        return true;
    }

    protected void back(int length) {
        pos -= length;
        ch = string.charAt(pos);
    }

    protected boolean hasNext() {
        return pos < string.length() - 1;
    }

    protected boolean checkWhitespace() {
        return Character.isWhitespace(ch);
    }

    protected boolean checkDigit() {
        return Character.isDigit(ch);
    }

    protected boolean checkVariable() {
        return test('x') || test('y') || test('z');
    }

    protected boolean between(final char left, final char right) {
        return left <= ch && ch <= right;
    }

    protected char take() {
        final char result = ch;
        ch = hasNext() ? string.charAt(++pos) : END;
        return result;
    }

    protected boolean take(final char expected) {
        if (test(expected)) {
            take();
            return true;
        } else {
            return false;
        }
    }

    protected void skipWhitespace() {
        while (checkWhitespace()) {
            take();
        }
    }

    protected IllegalArgumentException error(String message) {
        return new IllegalArgumentException(message);
    }
}
