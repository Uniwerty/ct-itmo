package calculator.lexer;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Lexer {
    private final String string;
    private final List<Matcher> tokenMatchers;
    private final Matcher skipMatcher;
    private int position;
    private String currentToken;

    public Lexer(String string) {
        this.string = string;
        tokenMatchers = new ArrayList<>();
        tokenMatchers.add(Pattern.compile("\\d+").matcher(string));
tokenMatchers.add(Pattern.compile("\\+").matcher(string));
tokenMatchers.add(Pattern.compile("\\/").matcher(string));
tokenMatchers.add(Pattern.compile("\\(").matcher(string));
tokenMatchers.add(Pattern.compile("\\$").matcher(string));
tokenMatchers.add(Pattern.compile("\\*").matcher(string));
tokenMatchers.add(Pattern.compile("\\)").matcher(string));
tokenMatchers.add(Pattern.compile("\\-").matcher(string));
tokenMatchers.add(Pattern.compile("").matcher(string));
skipMatcher = Pattern.compile("[ \\t\\r\\n]+").matcher(string);

    }

    public int getPosition() {
        return position;
    }

    public String getCurrentToken() {
        return currentToken;
    }

    public void takeNext() {
        skip();
        if (!hasNext()) {
            currentToken = "$";
            return;
        }
        for (Matcher matcher : tokenMatchers) {
            if (matcher.find(position) && matcher.start() == position) {
                currentToken = matcher.group();
                position = matcher.end();
                return;
            }
        }
    }

    private void skip() {
        if (skipMatcher.find(position) && skipMatcher.start() == position) {
            position = skipMatcher.end();
        }
    }

    private boolean hasNext() {
        return position < string.length();
    }
}
