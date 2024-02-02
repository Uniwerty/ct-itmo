package converter.visitor;

import converter.parser.ConverterBaseVisitor;
import converter.parser.ConverterParser;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class VariableMappingVisitor extends ConverterBaseVisitor<String> {
    private static final Map<String, String> BOOLEANS = Map.of(
            "False", "false",
            "True", "true"
    );
    private final Map<String, Integer> variableNumbers;

    public VariableMappingVisitor(Map<String, Integer> variableNumbers) {
        this.variableNumbers = variableNumbers;
    }

    @Override
    public String visitExpression(ConverterParser.ExpressionContext ctx) {
        if (ctx.mathExpression() != null) {
            return visitMathExpression(ctx.mathExpression());
        } else {
            return visitBoolExpression(ctx.boolExpression());
        }
    }

    @Override
    public String visitMathExpression(ConverterParser.MathExpressionContext ctx) {
        if (ctx.mathExpression() != null) {
            return String.format(
                    "%s %s %s",
                    visitMathExpression(ctx.mathExpression()),
                    ctx.sign.getText(),
                    visitSummand(ctx.summand())
            );
        }
        return visitSummand(ctx.summand());
    }

    @Override
    public String visitSummand(ConverterParser.SummandContext ctx) {
        if (ctx.summand() != null) {
            return String.format(
                    "%s %s %s",
                    visitSummand(ctx.summand()),
                    ctx.sign.getText(),
                    visitFactor(ctx.factor())
            );
        }
        return visitFactor(ctx.factor());
    }

    @Override
    public String visitFactor(ConverterParser.FactorContext ctx) {
        if (ctx.INT() != null) {
            return ctx.INT().getText();
        } else if (ctx.functionApplication() != null) {
            return visitFunctionApplication(ctx.functionApplication());
        } else if (ctx.variable() != null) {
            return "arg" + variableNumbers.get(ctx.variable().getText());
        } else {
            String minus = "";
            if (ctx.minus != null) {
                minus = ctx.minus.getText();
            }
            return String.format(
                    "%s(%s)",
                    minus,
                    visitMathExpression(ctx.mathExpression())
            );
        }
    }

    @Override
    public String visitBoolExpression(ConverterParser.BoolExpressionContext ctx) {
        if (ctx.boolExpression() != null) {
            return String.format(
                    "%s || %s",
                    visitBoolExpression(ctx.boolExpression()),
                    visitConjunction(ctx.conjunction())
            );
        }
        return visitConjunction(ctx.conjunction());
    }

    @Override
    public String visitConjunction(ConverterParser.ConjunctionContext ctx) {
        if (ctx.conjunction() != null) {
            return String.format(
                    "%s && %s",
                    visitConjunction(ctx.conjunction()),
                    visitBoolean(ctx.boolean_())
            );
        }
        return visitBoolean(ctx.boolean_());
    }

    @Override
    public String visitBoolean(ConverterParser.BooleanContext ctx) {
        if (ctx.BOOLEAN() != null) {
            return BOOLEANS.get(ctx.BOOLEAN().getText());
        } else if (ctx.variable() != null) {
            return "arg" + variableNumbers.get(ctx.variable().getText());
        } else if (ctx.comparison() != null) {
            return visitComparison(ctx.comparison());
        } else {
            String inversion = "";
            if (ctx.inversion != null) {
                inversion = "!";
            }
            return String.format(
                    "%s(%s)",
                    inversion,
                    visitBoolExpression(ctx.boolExpression())
            );
        }
    }

    @Override
    public String visitComparison(ConverterParser.ComparisonContext ctx) {
        return String.format(
                "%s %s %s",
                visitMathExpression(ctx.mathExpression(0)),
                ctx.sign.getText(),
                visitMathExpression(ctx.mathExpression(1))
        );
    }

    @Override
    public String visitFunctionApplication(ConverterParser.FunctionApplicationContext ctx) {
        List<String> arguments = new ArrayList<>();
        for (ConverterParser.ExpressionContext expression : ctx.expression()) {
            arguments.add(visitExpression(expression));
        }
        return String.format(
                "%s(%s)",
                ctx.NAME.getText(),
                String.join(", ", arguments)
        );
    }
}
