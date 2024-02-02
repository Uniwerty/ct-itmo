package converter;

import converter.parser.ConverterLexer;
import converter.parser.ConverterParser;
import converter.visitor.ProgramVisitor;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

public class FileConverter {
    private static final Charset CHARSET = StandardCharsets.UTF_8;

    public static void convert(Path fromFile, Path toFile) throws IOException {
        CharStream charStream = CharStreams.fromPath(fromFile, CHARSET);
        ConverterLexer lexer = new ConverterLexer(charStream);
        CommonTokenStream tokenStream = new CommonTokenStream(lexer);
        ConverterParser parser = new ConverterParser(tokenStream);
        Files.writeString(
                toFile,
                new ProgramVisitor().visitProgram(parser.program()),
                CHARSET
        );
    }
}
