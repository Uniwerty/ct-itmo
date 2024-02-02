package generator;

import generator.grammar.SpecificationBaseVisitor;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.text.ParseException;
import java.util.Objects;

public class Main extends SpecificationBaseVisitor<String> {
    public static void main(String[] args) throws IOException, ParseException, URISyntaxException {
//        Generator.generate(
//                Path.of(Objects.requireNonNull(Main.class.getResource("/CalculatorGrammar.txt")).toURI()),
//                System.getProperty("user.dir") + "\\src\\main\\java\\",
//                "calculator"
//        );
//        Generator.generate(
//                Path.of(Objects.requireNonNull(Main.class.getResource("/ArrayGrammar.txt")).toURI()),
//                System.getProperty("user.dir") + "\\src\\main\\java\\",
//                "array"
//        );
//        Generator.generate(
//                Path.of(Objects.requireNonNull(Main.class.getResource("/NotLL1.txt")).toURI()),
//                System.getProperty("user.dir") + "\\src\\main\\java\\",
//                "wrong"
//        );
//        System.out.println(new calculator.parser.Parser().parse("2 + 5 * 2").value);
        new array.parser.Parser().parse("var myArr : Tuple<A, D, B, C, D>");
    }
}
