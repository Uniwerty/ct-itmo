package parser;

import parser.syntax.Parser;

import java.text.ParseException;
import java.util.InputMismatchException;
import java.util.List;

public class Application {
    public static void main(String[] args) {
        try {
            System.out.println(new Parser().parse("var a : Map<String?, Int>").toGraphViz());
        } catch (ParseException e) {
            System.err.printf("%s at position %d%n", e.getMessage(), e.getErrorOffset());
        } catch (InputMismatchException e) {
            System.err.println(e.getMessage());
        }
    }
}
