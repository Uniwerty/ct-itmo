grammar Specification;

@header {
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import generator.Rule;
import generator.symbol.*;
}

@members {
public String skip;
public Set<Terminal> terminal = new HashSet<>(List.of(new Terminal("end", "\\\\$")));
public Set<String> nonterminal = new HashSet<>();
public List<Rule> rules = new ArrayList<>();
public Map<String, List<String>> ntArgs = new HashMap<>();
public Map<String, List<String>> ntReturns = new HashMap<>();
}

WHITESPACE : [ \t\r\n]+ -> skip;

fragment Letter : 'a'..'z' | 'A'..'Z';
fragment NameChar : Letter | '0'..'9' | '_';
Name : Letter (NameChar)*;
Attribute : Arg '::' Arg;
CodeBlock : '{' Code '}';
TERMINAL : '"' Text '"';
Argument : (NameChar | '.' | '_')+;

fragment Text : (~["] | ':')*;
fragment Arg : ~[:()]+;
fragment Code : ~[{}]*;

specification : skipRule attribute* rule+ EOF;

skipRule : term=TERMINAL { skip = $term.getText(); } '->' 'skip' ';';

attribute : nonterm=Name {
        String name = $nonterm.getText();
        ntArgs.put(name, new ArrayList<>());
        ntReturns.put(name, new ArrayList<>());
    }
    '(' (arg0=Attribute { ntArgs.get(name).add($arg0.getText()); } (',' arg=Attribute { ntArgs.get(name).add($arg.getText()); })*)? ')'
    ':'
    '(' (ret0=Attribute { ntReturns.get(name).add($ret0.getText()); } (',' ret=Attribute { ntReturns.get(name).add($ret.getText()); })*)? ')'
    ';'
    ;


rule : nonterm=Name {
            nonterminal.add($nonterm.getText());
            rules.add(new Rule(new NonTerminal($nonterm.getText())));
        }
        '->' inference ';';

inference :
    { int i = rules.size() - 1; }
    (
    name=Name '=' term=TERMINAL {
        String text = $term.getText().substring(1, $term.getText().length() - 1);
        Terminal cur = new Terminal($name.getText(), text);
        terminal.add(cur);
        rules.get(i).inference.add(cur);
    }
    | nonterm=Name {
        NonTerminal n = new NonTerminal($nonterm.getText());
        nonterminal.add($nonterm.getText());
    }
    ('(' arg0=Argument { n.arguments.add($arg0.getText()); } (',' arg=Argument { n.arguments.add($arg.getText()); })* ')')?
    {
        rules.get(i).inference.add(n);
    }
    | code=CodeBlock {
        String code = $code.getText().substring(1, $code.getText().length() - 1);
        rules.get(i).inference.add(new TranslationSymbol(code));
    }
    )+
    ;