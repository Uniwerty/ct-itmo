package parser.grammar.terminal;

public enum Token {
    VAR("var"),
    NAME("name"),
    SEPARATOR(":"),
    ARRAY("Array"),
    MAP("Map"),
    LEFT_BRACKET("<"),
    COMMA(","),
    RIGHT_BRACKET(">"),
    NULLABLE("?"),
    EMPTY("''"),
    END("$");

    private final String value;

    Token(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
