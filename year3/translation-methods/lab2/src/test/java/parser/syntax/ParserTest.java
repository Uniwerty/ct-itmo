package parser.syntax;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import parser.tree.Tree;

import java.text.ParseException;
import java.util.List;

import static parser.grammar.nonterminal.NonTerminal.*;
import static parser.grammar.terminal.Token.*;

public class ParserTest {
    private static final Parser parser = new Parser();

    @Test
    public void testPrimitiveArrayDeclaration() throws ParseException {
        Tree arrayTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                A.name(),
                                                List.of(
                                                        new Tree(ARRAY.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(NAME.getValue()),
                                                                        new Tree(
                                                                                T_PRIME.name(),
                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                        ),
                                                                        new Tree(
                                                                                N.name(),
                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                        )
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(
                                                                N.name(),
                                                                List.of(new Tree(EMPTY.getValue()))
                                                        )
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                arrayTree,
                List.of(
                        "var a : Array<Unit>",
                        "var a : Array<A>"
                )
        );
    }

    @Test
    public void testNestedArrayDeclaration() throws ParseException {
        Tree arrayTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                A.name(),
                                                List.of(
                                                        new Tree(ARRAY.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(
                                                                                C.name(),
                                                                                List.of(
                                                                                        new Tree(
                                                                                                A.name(),
                                                                                                List.of(
                                                                                                        new Tree(ARRAY.getValue()),
                                                                                                        new Tree(LEFT_BRACKET.getValue()),
                                                                                                        new Tree(
                                                                                                                T.name(),
                                                                                                                List.of(
                                                                                                                        new Tree(NAME.getValue()),
                                                                                                                        new Tree(
                                                                                                                                T_PRIME.name(),
                                                                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                                                                        ),
                                                                                                                        new Tree(
                                                                                                                                N.name(),
                                                                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                                                                        )
                                                                                                                )
                                                                                                        ),
                                                                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                                                                        new Tree(
                                                                                                                N.name(),
                                                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                                                        )
                                                                                                )
                                                                                        )
                                                                                )
                                                                        )
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(
                                                                N.name(),
                                                                List.of(new Tree(EMPTY.getValue()))
                                                        )
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                arrayTree,
                List.of(
                        "var a : Array<Array<String>>",
                        "var a : Array<Array<A>>"
                )
        );
    }

    @Test
    public void testParameterizedTypeArrayDeclaration() throws ParseException {
        Tree arrayTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                A.name(),
                                                List.of(
                                                        new Tree(ARRAY.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(NAME.getValue()),
                                                                        new Tree(
                                                                                T_PRIME.name(),
                                                                                List.of(
                                                                                        new Tree(LEFT_BRACKET.getValue()),
                                                                                        new Tree(
                                                                                                T.name(),
                                                                                                List.of(
                                                                                                        new Tree(NAME.getValue()),
                                                                                                        new Tree(
                                                                                                                T_PRIME.name(),
                                                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                                                        ),
                                                                                                        new Tree(
                                                                                                                N.name(),
                                                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                                                        )
                                                                                                )
                                                                                        ),
                                                                                        new Tree(RIGHT_BRACKET.getValue())
                                                                                )
                                                                        ),
                                                                        new Tree(
                                                                                N.name(),
                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                        )
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(
                                                                N.name(),
                                                                List.of(new Tree(EMPTY.getValue()))
                                                        )
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                arrayTree,
                List.of(
                        "var a: Array<Set<Int>>",
                        "var a: Array<A<B>>"
                )
        );
    }

    @Test
    public void testNullableTypeArrayDeclaration() throws ParseException {
        Tree arrayTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                A.name(),
                                                List.of(
                                                        new Tree(ARRAY.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(NAME.getValue()),
                                                                        new Tree(
                                                                                T_PRIME.name(),
                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                        ),
                                                                        new Tree(
                                                                                N.name(),
                                                                                List.of(new Tree(NULLABLE.getValue()))
                                                                        )
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(
                                                                N.name(),
                                                                List.of(new Tree(EMPTY.getValue()))
                                                        )
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                arrayTree,
                List.of(
                        "var a: Array<Int?>",
                        "var a: Array<A?>"
                )
        );
    }

    @Test
    public void testNullableArrayDeclaration() throws ParseException {
        Tree arrayTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                A.name(),
                                                List.of(
                                                        new Tree(ARRAY.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(NAME.getValue()),
                                                                        new Tree(
                                                                                T_PRIME.name(),
                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                        ),
                                                                        new Tree(
                                                                                N.name(),
                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                        )
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(
                                                                N.name(),
                                                                List.of(new Tree(NULLABLE.getValue()))
                                                        )
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                arrayTree,
                List.of(
                        "var a : Array<Int>?",
                        "var a : Array<A>?"
                )
        );
    }

    @Test
    public void testNullableTypeNullableArrayDeclaration() throws ParseException {
        Tree arrayTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                A.name(),
                                                List.of(
                                                        new Tree(ARRAY.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(NAME.getValue()),
                                                                        new Tree(
                                                                                T_PRIME.name(),
                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                        ),
                                                                        new Tree(
                                                                                N.name(),
                                                                                List.of(new Tree(NULLABLE.getValue()))
                                                                        )
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(
                                                                N.name(),
                                                                List.of(new Tree(NULLABLE.getValue()))
                                                        )
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                arrayTree,
                List.of(
                        "var a : Array<Int?>?",
                        "var a : Array<A?>?"
                )
        );
    }

    @Test
    public void testNullableParameterizedTypeArrayDeclaration() throws ParseException {
        Tree arrayTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                A.name(),
                                                List.of(
                                                        new Tree(ARRAY.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(NAME.getValue()),
                                                                        new Tree(
                                                                                T_PRIME.name(),
                                                                                List.of(
                                                                                        new Tree(LEFT_BRACKET.getValue()),
                                                                                        new Tree(
                                                                                                T.name(),
                                                                                                List.of(
                                                                                                        new Tree(NAME.getValue()),
                                                                                                        new Tree(
                                                                                                                T_PRIME.name(),
                                                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                                                        ),
                                                                                                        new Tree(
                                                                                                                N.name(),
                                                                                                                List.of(new Tree(NULLABLE.getValue()))
                                                                                                        )
                                                                                                )
                                                                                        ),
                                                                                        new Tree(RIGHT_BRACKET.getValue())
                                                                                )
                                                                        ),
                                                                        new Tree(
                                                                                N.name(),
                                                                                List.of(new Tree(NULLABLE.getValue()))
                                                                        )
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(
                                                                N.name(),
                                                                List.of(new Tree(EMPTY.getValue()))
                                                        )
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                arrayTree,
                List.of(
                        "var a: Array<Set<Int?>?>",
                        "var a: Array<A<B?>?>"
                )
        );
    }

    @Test
    public void testNullableTypeNestedNullableArrayDeclaration() throws ParseException {
        Tree arrayTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                A.name(),
                                                List.of(
                                                        new Tree(ARRAY.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(
                                                                                C.name(),
                                                                                List.of(
                                                                                        new Tree(
                                                                                                A.name(),
                                                                                                List.of(
                                                                                                        new Tree(ARRAY.getValue()),
                                                                                                        new Tree(LEFT_BRACKET.getValue()),
                                                                                                        new Tree(
                                                                                                                T.name(),
                                                                                                                List.of(
                                                                                                                        new Tree(NAME.getValue()),
                                                                                                                        new Tree(
                                                                                                                                T_PRIME.name(),
                                                                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                                                                        ),
                                                                                                                        new Tree(
                                                                                                                                N.name(),
                                                                                                                                List.of(new Tree(NULLABLE.getValue()))
                                                                                                                        )
                                                                                                                )
                                                                                                        ),
                                                                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                                                                        new Tree(
                                                                                                                N.name(),
                                                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                                                        )
                                                                                                )
                                                                                        )
                                                                                )
                                                                        )
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(
                                                                N.name(),
                                                                List.of(new Tree(NULLABLE.getValue()))
                                                        )
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                arrayTree,
                List.of(
                        "var a : Array<Array<String?>>?",
                        "var a : Array<Array<A?>>?"
                )
        );
    }

    @Test
    public void testPrimitiveMapDeclaration() throws ParseException {
        Tree mapTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                M.name(),
                                                List.of(
                                                        new Tree(MAP.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(NAME.getValue()),
                                                                        new Tree(T_PRIME.name(), List.of(new Tree(EMPTY.getValue()))),
                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                )
                                                        ),
                                                        new Tree(COMMA.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(NAME.getValue()),
                                                                        new Tree(T_PRIME.name(), List.of(new Tree(EMPTY.getValue()))),
                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                mapTree,
                List.of(
                        "var a : Map<Int, String>",
                        "var a : Map<A, B>"
                )
        );
    }

    @Test
    public void testNestedMapArrayDeclaration() throws ParseException {
        Tree mapTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                M.name(),
                                                List.of(
                                                        new Tree(MAP.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(
                                                                                C.name(),
                                                                                List.of(
                                                                                        new Tree(
                                                                                                A.name(),
                                                                                                List.of(
                                                                                                        new Tree(ARRAY.getValue()),
                                                                                                        new Tree(LEFT_BRACKET.getValue()),
                                                                                                        new Tree(
                                                                                                                T.name(),
                                                                                                                List.of(
                                                                                                                        new Tree(NAME.getValue()),
                                                                                                                        new Tree(T_PRIME.name(), List.of(new Tree(EMPTY.getValue()))),
                                                                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                                                                )
                                                                                                        ),
                                                                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                                                )
                                                                                        )
                                                                                )
                                                                        )
                                                                )
                                                        ),
                                                        new Tree(COMMA.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(
                                                                                C.name(),
                                                                                List.of(
                                                                                        new Tree(
                                                                                                M.name(),
                                                                                                List.of(
                                                                                                        new Tree(MAP.getValue()),
                                                                                                        new Tree(LEFT_BRACKET.getValue()),
                                                                                                        new Tree(
                                                                                                                T.name(),
                                                                                                                List.of(
                                                                                                                        new Tree(NAME.getValue()),
                                                                                                                        new Tree(T_PRIME.name(), List.of(new Tree(EMPTY.getValue()))),
                                                                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                                                                )
                                                                                                        ),
                                                                                                        new Tree(COMMA.getValue()),
                                                                                                        new Tree(
                                                                                                                T.name(),
                                                                                                                List.of(
                                                                                                                        new Tree(NAME.getValue()),
                                                                                                                        new Tree(T_PRIME.name(), List.of(new Tree(EMPTY.getValue()))),
                                                                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                                                                )
                                                                                                        ),
                                                                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                                                )
                                                                                        )
                                                                                )
                                                                        )
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                mapTree,
                List.of(
                        "var a : Map<Array<Int>, Map<Int, Int>>",
                        "var a : Map<Array<A>, Map<B, C>>"
                )
        );
    }

    @Test
    public void testParameterizedTypeMapDeclaration() throws ParseException {
        Tree mapTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                M.name(),
                                                List.of(
                                                        new Tree(MAP.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(NAME.getValue()),
                                                                        new Tree(
                                                                                T_PRIME.name(),
                                                                                List.of(
                                                                                        new Tree(LEFT_BRACKET.getValue()),
                                                                                        new Tree(
                                                                                                T.name(),
                                                                                                List.of(
                                                                                                        new Tree(NAME.getValue()),
                                                                                                        new Tree(T_PRIME.name(), List.of(new Tree(EMPTY.getValue()))),
                                                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                                                )
                                                                                        ),
                                                                                        new Tree(RIGHT_BRACKET.getValue())
                                                                                )
                                                                        ),
                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                )
                                                        ),
                                                        new Tree(COMMA.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(NAME.getValue()),
                                                                        new Tree(T_PRIME.name(), List.of(new Tree(EMPTY.getValue()))),
                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                mapTree,
                List.of(
                        "var a : Map<Set<Int>, String>",
                        "var a : Map<A<B>, C>"
                )
        );
    }

    @Test
    public void testNullableTypeNestedNullableMapDeclaration() throws ParseException {
        Tree arrayTree = new Tree(
                S.name(),
                List.of(
                        new Tree(VAR.getValue()),
                        new Tree(NAME.getValue()),
                        new Tree(SEPARATOR.getValue()),
                        new Tree(
                                C.name(),
                                List.of(
                                        new Tree(
                                                M.name(),
                                                List.of(
                                                        new Tree(MAP.getValue()),
                                                        new Tree(LEFT_BRACKET.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(NAME.getValue()),
                                                                        new Tree(T_PRIME.name(), List.of(new Tree(EMPTY.getValue()))),
                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                )
                                                        ),
                                                        new Tree(COMMA.getValue()),
                                                        new Tree(
                                                                T.name(),
                                                                List.of(
                                                                        new Tree(
                                                                                C.name(),
                                                                                List.of(
                                                                                        new Tree(
                                                                                                M.name(),
                                                                                                List.of(
                                                                                                        new Tree(MAP.getValue()),
                                                                                                        new Tree(LEFT_BRACKET.getValue()),
                                                                                                        new Tree(
                                                                                                                T.name(),
                                                                                                                List.of(
                                                                                                                        new Tree(NAME.getValue()),
                                                                                                                        new Tree(T_PRIME.name(), List.of(new Tree(EMPTY.getValue()))),
                                                                                                                        new Tree(N.name(), List.of(new Tree(NULLABLE.getValue())))
                                                                                                                )
                                                                                                        ),
                                                                                                        new Tree(COMMA.getValue()),
                                                                                                        new Tree(
                                                                                                                T.name(),
                                                                                                                List.of(
                                                                                                                        new Tree(NAME.getValue()),
                                                                                                                        new Tree(T_PRIME.name(), List.of(new Tree(EMPTY.getValue()))),
                                                                                                                        new Tree(N.name(), List.of(new Tree(EMPTY.getValue())))
                                                                                                                )
                                                                                                        ),
                                                                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                                                                        new Tree(
                                                                                                                N.name(),
                                                                                                                List.of(new Tree(EMPTY.getValue()))
                                                                                                        )
                                                                                                )
                                                                                        )
                                                                                )
                                                                        )
                                                                )
                                                        ),
                                                        new Tree(RIGHT_BRACKET.getValue()),
                                                        new Tree(
                                                                N.name(),
                                                                List.of(new Tree(NULLABLE.getValue()))
                                                        )
                                                )
                                        )
                                )
                        )
                )
        );
        testEqualsMany(
                arrayTree,
                List.of(
                        "var a : Map<Int, Map<String?, String>>?",
                        "var a : Map<A, Map<B?, B>>?"
                )
        );
    }

    @Test
    public void testInvalidDeclarations() {
        List<Executable> cases = List.of(
                () -> parser.parse(""),
                () -> parser.parse("var"),
                () -> parser.parse("var a"),
                () -> parser.parse("var a :"),
                () -> parser.parse("var a : A"),
                () -> parser.parse("var a : Array"),
                () -> parser.parse("var a : Array<"),
                () -> parser.parse("var a : Array<>"),
                () -> parser.parse("var a : Array<Int"),
                () -> parser.parse("var a : Array<Int<"),
                () -> parser.parse("var a : Array>Int>"),
                () -> parser.parse("val a : Array<Int>"),
                () -> parser.parse("var : Array<Int>"),
                () -> parser.parse("var a Array<Int>"),
                () -> parser.parse("var a : array<Int>"),
                () -> parser.parse("var a : A<Int>"),
                () -> parser.parse("var a : Array<Array>"),
                () -> parser.parse("var : Array<Int>"),
                () -> parser.parse("a : Array<Int>"),
                () -> parser.parse("var a : <Int>"),
                () -> parser.parse("var a : <Int>Array"),
                () -> parser.parse("var a Array : Int"),
                () -> parser.parse("var a : Array<Array<>"),
                () -> parser.parse("var a : Array<Array<>>"),
                () -> parser.parse("var a : Array<Array<>Int>"),
                () -> parser.parse("var a : Array<Int<>>"),
                () -> parser.parse("var a : Array<Int??>"),
                () -> parser.parse("var a : Array<?Int>"),
                () -> parser.parse("var a : Array?<Int>"),
                () -> parser.parse("var a : ?Array<Int>"),
                () -> parser.parse("var a? : Array<Int>"),
                () -> parser.parse("var ?a : Array<Int>"),
                () -> parser.parse("var? a : Array<Int>"),
                () -> parser.parse("var ? : Array<Int>"),
                () -> parser.parse("var a : Array<?>"),
                () -> parser.parse("var a : Map"),
                () -> parser.parse("var a : Map<"),
                () -> parser.parse("var a : Map<Int"),
                () -> parser.parse("var a : Map<Int,"),
                () -> parser.parse("var a : Map<Int,Int"),
                () -> parser.parse("var a : Map<Int,Int>>"),
                () -> parser.parse("var a : Map<<Int,Int>"),
                () -> parser.parse("var a : Map<<Int,Int>>"),
                () -> parser.parse("var a : Map<,Int,Int>"),
                () -> parser.parse("var a : Map<Int,Int,>"),
                () -> parser.parse("var a : Map<Int Int>"),
                () -> parser.parse("var a : Map<Int>"),
                () -> parser.parse("var a : Map<Int ? Int>"),
                () -> parser.parse("var a : Map<?Int, Int>"),
                () -> parser.parse("var a : Map<Int, ?Int>")
        );
        cases.forEach(task -> Assertions.assertThrows(ParseException.class, task));
    }

    private void testEqualsMany(Tree expected, List<String> testStrings) throws ParseException {
        for (String string : testStrings) {
            Assertions.assertEquals(
                    expected,
                    parser.parse(string)
            );
        }
    }
}
