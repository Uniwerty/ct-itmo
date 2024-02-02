package parser.lexic;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import parser.grammar.terminal.Token;

import java.text.ParseException;
import java.util.List;

import static parser.grammar.terminal.Token.*;

public class LexerTest {
    @Test
    public void testKeywordTokens() throws ParseException {
        List<String> strings = List.of("var", ":", "Array", "Map", "<", ",", ">", "?");
        List<Token> tokens = List.of(VAR, SEPARATOR, ARRAY, MAP, LEFT_BRACKET, COMMA, RIGHT_BRACKET, NULLABLE);
        for (int i = 0; i < tokens.size(); i++) {
            Lexer lexer = new Lexer(strings.get(i));
            lexer.takeNextToken();
            Assertions.assertEquals(tokens.get(i), lexer.getCurrentToken());
        }
    }

    @Test
    public void testValidNames() throws ParseException {
        List<String> names = List.of(
                "name",
                "Name",
                "NAME",
                "Name123",
                "array123name",
                "array_name",
                "_name",
                "varvar",
                "ArrayArray",
                "MapMap"
        );
        for (String name : names) {
            Lexer lexer = new Lexer(name);
            lexer.takeNextToken();
            Assertions.assertEquals(NAME, lexer.getCurrentToken());
        }
    }

    @Test
    public void testInvalidNames() {
        List<String> names = List.of(
                "123name",
                "!name",
                "-name",
                ".name"
        );
        for (String name : names) {
            Lexer lexer = new Lexer(name);
            Assertions.assertThrows(ParseException.class, lexer::takeNextToken);
        }
    }

    @Test
    public void testMultipleTokens() throws ParseException {
        List<String> strings = List.of(
                "<<>>",
                "abc def ghi",
                "Array<Array<Int>>",
                "Array<String?>?",
                "var array:Array<Int>",
                "var map:Map<Int, String>"
        );
        List<List<Token>> tokens = List.of(
                List.of(LEFT_BRACKET, LEFT_BRACKET, RIGHT_BRACKET, RIGHT_BRACKET),
                List.of(NAME, NAME, NAME),
                List.of(ARRAY, LEFT_BRACKET, ARRAY, LEFT_BRACKET, NAME, RIGHT_BRACKET, RIGHT_BRACKET),
                List.of(ARRAY, LEFT_BRACKET, NAME, NULLABLE, RIGHT_BRACKET, NULLABLE),
                List.of(VAR, NAME, SEPARATOR, ARRAY, LEFT_BRACKET, NAME, RIGHT_BRACKET),
                List.of(VAR, NAME, SEPARATOR, MAP, LEFT_BRACKET, NAME, COMMA, NAME, RIGHT_BRACKET)
        );
        for (int i = 0; i < strings.size(); i++) {
            Lexer lexer = new Lexer(strings.get(i));
            Assertions.assertEquals(tokens.get(i), lexer.getTokensList());
        }
    }

    @Test
    public void testWhitespaceSkipping() throws ParseException {
        List<String> strings = List.of(
                "   var \t   \n\r   ",
                "\n  var \r   var \n \t  var   \t",
                "   var\n\n\n   a  \n :  \t\t  Array \n  < \t  b\r   \n>\n   \t"
        );
        List<List<Token>> tokens = List.of(
                List.of(VAR),
                List.of(VAR, VAR, VAR),
                List.of(VAR, NAME, SEPARATOR, ARRAY, LEFT_BRACKET, NAME, RIGHT_BRACKET)
        );
        for (int i = 0; i < strings.size(); i++) {
            Lexer lexer = new Lexer(strings.get(i));
            Assertions.assertEquals(tokens.get(i), lexer.getTokensList());
        }
    }
}
