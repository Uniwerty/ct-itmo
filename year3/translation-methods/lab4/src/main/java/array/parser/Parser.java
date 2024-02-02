package array.parser;

import array.lexer.Lexer;

import java.text.ParseException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Parser {
    private static final Pattern tuplePattern = Pattern.compile("Tuple");
    private static final Pattern namePattern = Pattern.compile("\\w+");
    private static final Pattern lparPattern = Pattern.compile("<");
    private static final Pattern commaPattern = Pattern.compile(",");
    private static final Pattern nullablePattern = Pattern.compile("\\?");
    private static final Pattern arrayPattern = Pattern.compile("Array");
    private static final Pattern rparPattern = Pattern.compile(">");
    private static final Pattern sepPattern = Pattern.compile(":");
    private static final Pattern endPattern = Pattern.compile("\\$");
    private static final Pattern varPattern = Pattern.compile("var");
    private static final Pattern emptyPattern = Pattern.compile("");
    private Lexer lexer;

    public S parse(String string) throws ParseException {
        lexer = new Lexer(string);
        lexer.takeNext();
        return parseS();
    }

    public P parseP() throws ParseException {
        P P_0 = new P();
        if (tuplePattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = tuplePattern.matcher(lexer.getCurrentToken());
            m0.find();
            String tuple = m0.group();
            lexer.takeNext();
            Matcher m1 = lparPattern.matcher(lexer.getCurrentToken());
            m1.find();
            String lpar = m1.group();
            lexer.takeNext();
            E E_0 = parseE();
            Matcher m2 = rparPattern.matcher(lexer.getCurrentToken());
            m2.find();
            String rpar = m2.group();
            lexer.takeNext();
            N N_0 = parseN();
            return P_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public A parseA() throws ParseException {
        A A_0 = new A();
        if (arrayPattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = arrayPattern.matcher(lexer.getCurrentToken());
            m0.find();
            String array = m0.group();
            lexer.takeNext();
            Matcher m1 = lparPattern.matcher(lexer.getCurrentToken());
            m1.find();
            String lpar = m1.group();
            lexer.takeNext();
            T T_0 = parseT();
            Matcher m2 = rparPattern.matcher(lexer.getCurrentToken());
            m2.find();
            String rpar = m2.group();
            lexer.takeNext();
            N N_0 = parseN();
            return A_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public S parseS() throws ParseException {
        S S_0 = new S();
        if (varPattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = varPattern.matcher(lexer.getCurrentToken());
            m0.find();
            String var = m0.group();
            lexer.takeNext();
            Matcher m1 = namePattern.matcher(lexer.getCurrentToken());
            m1.find();
            String name = m1.group();
            lexer.takeNext();
            Matcher m2 = sepPattern.matcher(lexer.getCurrentToken());
            m2.find();
            String sep = m2.group();
            lexer.takeNext();
            C C_0 = parseC();
            return S_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public C parseC() throws ParseException {
        C C_0 = new C();
        if (arrayPattern.matcher(lexer.getCurrentToken()).matches()) {
            A A_0 = parseA();
            return C_0;
        }
        if (tuplePattern.matcher(lexer.getCurrentToken()).matches()) {
            P P_0 = parseP();
            return C_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public T parseT() throws ParseException {
        T T_0 = new T();
        if (namePattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = namePattern.matcher(lexer.getCurrentToken());
            m0.find();
            String name = m0.group();
            lexer.takeNext();
            T2 T2_0 = parseT2();
            N N_0 = parseN();
            return T_0;
        }
        if (arrayPattern.matcher(lexer.getCurrentToken()).matches()) {
            A A_0 = parseA();
            return T_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public E parseE() throws ParseException {
        E E_0 = new E();
        if (arrayPattern.matcher(lexer.getCurrentToken()).matches() || namePattern.matcher(lexer.getCurrentToken()).matches()) {
            T T_0 = parseT();
            E2 E2_0 = parseE2();
            return E_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public E2 parseE2() throws ParseException {
        E2 E2_0 = new E2();
        if (commaPattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = commaPattern.matcher(lexer.getCurrentToken());
            m0.find();
            String comma = m0.group();
            lexer.takeNext();
            T T_0 = parseT();
            E2 E2_1 = parseE2();
            return E2_0;
        }
        if (rparPattern.matcher(lexer.getCurrentToken()).matches()) {
            return E2_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public N parseN() throws ParseException {
        N N_0 = new N();
        if (nullablePattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = nullablePattern.matcher(lexer.getCurrentToken());
            m0.find();
            String nullable = m0.group();
            lexer.takeNext();
            return N_0;
        }
        if (endPattern.matcher(lexer.getCurrentToken()).matches() || commaPattern.matcher(lexer.getCurrentToken()).matches() || rparPattern.matcher(lexer.getCurrentToken()).matches()) {
            return N_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }

    public T2 parseT2() throws ParseException {
        T2 T2_0 = new T2();
        if (lparPattern.matcher(lexer.getCurrentToken()).matches()) {
            Matcher m0 = lparPattern.matcher(lexer.getCurrentToken());
            m0.find();
            String lpar = m0.group();
            lexer.takeNext();
            T T_0 = parseT();
            Matcher m1 = rparPattern.matcher(lexer.getCurrentToken());
            m1.find();
            String rpar = m1.group();
            lexer.takeNext();
            return T2_0;
        }
        if (commaPattern.matcher(lexer.getCurrentToken()).matches() || rparPattern.matcher(lexer.getCurrentToken()).matches() || nullablePattern.matcher(lexer.getCurrentToken()).matches()) {
            return T2_0;
        }
        throw new ParseException("Unexpected token: " + lexer.getCurrentToken(), lexer.getPosition());
    }


    public static class P {

    }

    public static class A {

    }

    public static class S {

    }

    public static class C {

    }

    public static class T {

    }

    public static class E {

    }

    public static class E2 {

    }

    public static class N {

    }

    public static class T2 {

    }
}
