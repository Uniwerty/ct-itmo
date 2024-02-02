package generator;

import generator.grammar.SpecificationLexer;
import generator.grammar.SpecificationParser;
import generator.lexer.LexerGenerator;
import generator.parser.ParserGenerator;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;

public class Generator {
    public static void generate(Path grammarFile, String directory, String packageName) throws IOException {
        CharStream charStream = CharStreams.fromPath(
                grammarFile,
                StandardCharsets.UTF_8
        );
        SpecificationLexer lexer = new SpecificationLexer(charStream);
        CommonTokenStream tokenStream = new CommonTokenStream(lexer);
        SpecificationParser parser = new SpecificationParser(tokenStream);
        parser.specification();
        String path = directory + packageName;
        String lexerPath = path + "\\lexer";
        String parserPath = path + "\\parser";
        if (Files.notExists(Path.of(lexerPath))) {
            Files.createDirectories(Path.of(lexerPath));
        }
        if (Files.notExists(Path.of(parserPath))) {
            Files.createDirectories(Path.of(parserPath));
        }
        LexerGenerator lexerGenerator = new LexerGenerator(
                new ArrayList<>(parser.terminal.stream().map(t -> t.value).toList()),
                parser.skip
        );
        ParserGenerator parserGenerator = new ParserGenerator(
                parser.terminal,
                parser.nonterminal,
                parser.rules,
                parser.ntArgs,
                parser.ntReturns
        );
        if (!parserGenerator.checkLL1()) {
            System.err.println("The specified grammar is not LL(1)!");
            return;
        }
        lexerGenerator.generateFile(
                Path.of(directory + packageName + "\\lexer\\Lexer.java"),
                packageName
        );
        parserGenerator.generateFile(
                Path.of(directory + packageName + "\\parser\\Parser.java"),
                packageName
        );

    }
}
