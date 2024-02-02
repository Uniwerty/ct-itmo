grammar Converter;

@header {
    import java.util.List;
    import java.util.ArrayList;
    import java.util.Map;
    import java.util.HashMap;
    import java.util.Set;
    import java.util.HashSet;
    import converter.exception.ConversionException;
}

@members {
    Map<String, List<String>> functions = new HashMap<>();
}

WHITESPACE : [ \t\r\n]+ -> skip;

fragment Letter : 'a'..'z' | 'A'..'Z';
fragment DigitStart : '1'..'9';
fragment Digit : '0' | DigitStart;

INT : ('+'|'-')?('0' | DigitStart(Digit)*);
BOOLEAN : 'True' | 'False';

NAME : NameStartChar (NameChar)*;
fragment NameStartChar : Letter | '_';
fragment NameChar : NameStartChar | Digit;

program : functionDeclaration+ EOF;

functionDeclaration
    locals[
        String name,
        List<String> argumentTypes = new ArrayList<>()
    ]
    :
    functionSignature { functions.put($name, $argumentTypes); }
    functionBody;

functionSignature
    :
    NAME '::' functionType
    {
        if (functions.containsKey($NAME.text)) {
            throw new ConversionException("Function already defined: " + $NAME.text);
        }
        $functionDeclaration::name = $NAME.text;
    };

functionType : typeName ('->' typeName)*;

typeName
    :
    ('Int' | 'Bool')
    { $functionDeclaration::argumentTypes.add($ctx.getText()); }
    ;

functionBody : (functionCase)+;

functionCase
    locals[
        Map<String, Integer> variables = new HashMap<>(),
        Set<Integer> expressionNumbers = new HashSet<>(),
        int argumentNumber = 0
    ]
    :
    functionArgumentsCase (functionResult | functionGuards);

functionArgumentsCase
    :
    NAME {
        if (!$functionDeclaration::name.equals($NAME.text)) {
            throw new ConversionException("Wrong function name: " + $NAME.text);
        }
    }
    (functionArgument { $functionCase::argumentNumber++; })*
    {
        if ($functionCase::argumentNumber != $functionDeclaration::argumentTypes.size() - 1) {
            throw new ConversionException("Wrong arguments number for function " + $NAME.text);
        }
    };

functionArgument
    :
    NAME { $functionCase::variables.put($NAME.text, $functionCase::argumentNumber); }
    | expression[$functionDeclaration::argumentTypes.get($functionCase::argumentNumber)]
    { $functionCase::expressionNumbers.add($functionCase::argumentNumber); };

functionResult
    :
    '='
    { List<String> argumentTypes = $functionDeclaration::argumentTypes; }
    expression[argumentTypes.get(argumentTypes.size() - 1)];

functionGuards : condition+;

condition : '|' boolExpression functionResult;

expression[String expectedType]
    :
    { $expectedType.equals("Int") }? mathExpression
    | { $expectedType.equals("Bool") }? boolExpression;
    catch[NoViableAltException e]
    { throw new ConversionException($expectedType + " expression expected: " + $ctx.getText()); }

mathExpression
    :
    mathExpression sign=('+'|'-') summand
    | summand;

summand
    :
    summand sign=('*'|'/') factor
    | factor;

factor
    :
    INT
    | functionApplication["Int"]
    | variable["Int"]
    | (minus='-')? '(' mathExpression ')';

boolExpression
    :
    boolExpression '||' conjunction
    | conjunction;

conjunction
    :
    conjunction '&&' boolean
    | boolean;

boolean
    :
    BOOLEAN
    | functionApplication["Bool"]
    | variable["Bool"]
    | comparison
    | (inversion='!')? '(' boolExpression ')';

comparison : mathExpression sign=('<'|'<='|'>'|'>='|'=='|'!=') mathExpression;

variable[String expectedType]
    :
    NAME
    {
        Map<String, Integer> variables = $functionCase::variables;
        String name = $NAME.text;
        if (!variables.containsKey(name)) {
            throw new ConversionException("Unknown variable " + name + " provided");
        }
        String type = $functionDeclaration::argumentTypes.get(variables.get(name));
        if (!type.equals($expectedType)) {
            throw new ConversionException(
                String.format(
                    "Wrong variable %s type: %s expected but %s provided",
                    $NAME.text,
                    $expectedType,
                    type
                )
            );
        }
    };

functionApplication[String expectedType]
    :
    NAME {
        if (!functions.containsKey($NAME.text)) {
            throw new ConversionException("Unknown function expression: " + $NAME.text);
        }
    }
    {
        List<String> argumentTypes = functions.get($NAME.text);
        int argumentNumber = 0;
    }
    '(' (expression[argumentTypes.get(argumentNumber++)]
            (',' expression[argumentTypes.get(argumentNumber++)])*
        )?
    ')'
    {
        List<String> functionTypes = functions.get($NAME.text);
        if ($ctx.expression().size() != functionTypes.size() - 1) {
            throw new ConversionException(
                String.format(
                    "Wrong arguments number for function %s: %s",
                     $NAME.text,
                     $ctx.getText()
                )
            );
        }
        String type = functionTypes.get(functionTypes.size() - 1);
        if (!type.equals($expectedType)) {
            throw new ConversionException(
                String.format(
                    "Wrong function application %s type: %s expected but %s provided",
                    $NAME.text,
                    $expectedType,
                    type
                )
            );
        }
    };
