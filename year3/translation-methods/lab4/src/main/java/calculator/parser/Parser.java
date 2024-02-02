package calculator.parser;

import calculator.lexer.Lexer;

import java.text.ParseException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Parser {
    private static final Pattern plusPattern = Pattern.compile("\\+");
    private static final Pattern divPattern = Pattern.compile("\\/");
    private static final Pattern numberPattern = Pattern.compile("\\d+");
    private static final Pattern lparPattern = Pattern.compile("\\(");
    private static final Pattern endPattern = Pattern.compile("\\$");
    private static final Pattern mulPattern = Pattern.compile("\\*");
    private static final Pattern rparPattern = Pattern.compile("\\)");
    private static final Pattern minusPattern = Pattern.compile("\\-");
    private static final Pattern emptyPattern = Pattern.compile("");
    private Lexer lexer;

    public S parse(String string) throws ParseException {
        lexer = new Lexer(string);
        lexer.takeNext();
        return parseS();
    }

    public S parseS() throws ParseException {
        S S_0 = new S();
        if (lparPattern.matcher(lexer.getCurrentToken()).matches() || numberPattern.matcher(lexer.getCurrentToken()).matches()) {
            T T_0 = parseT();
            E2 E2_0 = parseE2(T_0.value);
            S_0.value = E2_0.value;
            return S_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public T parseT() throws ParseException {
        T T_0 = new T();
        if (lparPattern.matcher(lexer.getCurrentToken()).matches() || numberPattern.matcher(lexer.getCurrentToken()).matches()) {
            F F_0 = parseF();
            T2 T2_0 = parseT2(F_0.value);
            T_0.value = T2_0.value;
            return T_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public F parseF() throws ParseException {
        F F_0 = new F();
        if (numberPattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = numberPattern.matcher(lexer.getCurrentToken());
            m0.find();
            String number = m0.group();
            lexer.takeNext();
            F_0.value = Integer.parseInt(number);
            return F_0;
        }
        if (lparPattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = lparPattern.matcher(lexer.getCurrentToken());
            m0.find();
            String lpar = m0.group();
            lexer.takeNext();
            S S_0 = parseS();
            Matcher m1 = rparPattern.matcher(lexer.getCurrentToken());
            m1.find();
            String rpar = m1.group();
            lexer.takeNext();
            F_0.value = S_0.value;
            return F_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public E2 parseE2(int acc) throws ParseException {
        E2 E2_0 = new E2();
        if (plusPattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = plusPattern.matcher(lexer.getCurrentToken());
            m0.find();
            String plus = m0.group();
            lexer.takeNext();
            T T_0 = parseT();
            int _cur = acc + T_0.value;
            E2 E2_1 = parseE2(_cur);
            E2_0.value = E2_1.value;
            return E2_0;
        }
        if (minusPattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = minusPattern.matcher(lexer.getCurrentToken());
            m0.find();
            String minus = m0.group();
            lexer.takeNext();
            T T_0 = parseT();
            int _cur = acc - T_0.value;
            E2 E2_1 = parseE2(_cur);
            E2_0.value = E2_1.value;
            return E2_0;
        }
        if (endPattern.matcher(lexer.getCurrentToken()).matches() || rparPattern.matcher(lexer.getCurrentToken()).matches()) {
            E2_0.value = acc;
            return E2_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public T2 parseT2(int acc) throws ParseException {
        T2 T2_0 = new T2();
        if (mulPattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = mulPattern.matcher(lexer.getCurrentToken());
            m0.find();
            String mul = m0.group();
            lexer.takeNext();
            F F_0 = parseF();
            int _cur = acc * F_0.value;
            T2 T2_1 = parseT2(_cur);
            T2_0.value = T2_1.value;
            return T2_0;
        }
        if (divPattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = divPattern.matcher(lexer.getCurrentToken());
            m0.find();
            String div = m0.group();
            lexer.takeNext();
            F F_0 = parseF();
            int _cur = acc / F_0.value;
            T2 T2_1 = parseT2(_cur);
            T2_0.value = T2_1.value;
            return T2_0;
        }
        if (endPattern.matcher(lexer.getCurrentToken()).matches() || rparPattern.matcher(lexer.getCurrentToken()).matches() || plusPattern.matcher(lexer.getCurrentToken()).matches() || minusPattern.matcher(lexer.getCurrentToken()).matches()) {
            T2_0.value = acc;
            return T2_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }


    public static class S {
        public int value;
    }

    public static class T {
        public int value;
    }

    public static class F {
        public int value;
    }

    public static class E2 {
        public int value;
    }

    public static class T2 {
        public int value;
    }
}
