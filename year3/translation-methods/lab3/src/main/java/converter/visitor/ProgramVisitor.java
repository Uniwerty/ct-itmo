package converter.visitor;

import converter.parser.ConverterBaseVisitor;
import converter.parser.ConverterParser;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ProgramVisitor extends ConverterBaseVisitor<String> {
    private static final String LINE_SEPARATOR = System.lineSeparator();
    private static final String LINE_OFFSET_SEPARATOR = LINE_SEPARATOR + "    ";
    private static final String LINE_SKIP_SEPARATOR = LINE_SEPARATOR + LINE_SEPARATOR + "    ";
    private static final Map<String, String> PRIMITIVE_TYPES = Map.of(
            "Bool", "boolean",
            "Int", "int",
            "Double", "double"
    );

    @Override
    public String visitProgram(converter.parser.ConverterParser.ProgramContext ctx) {
        List<String> functions = new ArrayList<>();
        for (int i = 0; i < ctx.getChildCount() - 1; i++) {
            functions.add(addOffset(visitFunctionDeclaration(ctx.functionDeclaration(i))));
        }
        return String.format(
                "public class Program {%n    %s%n}",
//                addOffset(
                String.join(LINE_SKIP_SEPARATOR, functions)
//                )
        );
    }

    @Override
    public String visitFunctionDeclaration(ConverterParser.FunctionDeclarationContext ctx) {
        return String.format(
                "%s {%n    %s%n}",
                visitFunctionSignature(ctx.functionSignature()),
                addOffset(visitFunctionBody(ctx.functionBody()))
        );
    }

    @Override
    public String visitFunctionSignature(ConverterParser.FunctionSignatureContext ctx) {
        ConverterParser.FunctionTypeContext functionTypeContext = ctx.functionType();
        List<ConverterParser.TypeNameContext> types = functionTypeContext.typeName();
        List<String> arguments = new ArrayList<>();
        for (int i = 0; i < types.size() - 1; i++) {
            arguments.add(String.format("%s arg%d", visitTypeName(types.get(i)), i));
        }
        return String.format(
                "public %s %s(%s)",
                getTypeName(types.get(types.size() - 1).getText()),
                ctx.NAME().getText(),
                String.join(", ", arguments)
        );
    }

    @Override
    public String visitFunctionBody(ConverterParser.FunctionBodyContext ctx) {
        List<String> cases = new ArrayList<>();
        for (int i = 0; i < ctx.getChildCount(); i++) {
            cases.add(visitFunctionCase(ctx.functionCase(i)));
        }
        return String.join(LINE_SEPARATOR, cases);
    }

    @Override
    public String visitFunctionCase(ConverterParser.FunctionCaseContext ctx) {
        if (ctx.expressionNumbers.isEmpty()) {
            return processSimpleCase(ctx);
        } else {
            return processConditionalCase(ctx);
        }
    }

    @Override
    public String visitTypeName(ConverterParser.TypeNameContext ctx) {
        return getTypeName(ctx.getText());
    }

    private String processSimpleCase(ConverterParser.FunctionCaseContext ctx) {
        if (ctx.functionResult() != null) {
            return processReturn(ctx.functionResult());
        } else {
            return processGuards(ctx.functionGuards());
        }
    }

    private String processConditionalCase(ConverterParser.FunctionCaseContext ctx) {
        ConverterParser.FunctionArgumentsCaseContext caseContext = ctx.functionArgumentsCase();
        List<String> conditions = new ArrayList<>();
        for (int i = 0; i < ctx.argumentNumber; i++) {
            if (ctx.expressionNumbers.contains(i)) {
                conditions.add(
                        String.format(
                                "arg%d == %s",
                                i,
                                new VariableMappingVisitor(ctx.variables)
                                        .visitExpression(
                                                caseContext.functionArgument(i).expression()
                                        )
                        )
                );
            }
        }
        String functionReturnStatement;
        if (ctx.functionResult() != null) {
            functionReturnStatement = processReturn(ctx.functionResult());
        } else {
            functionReturnStatement = processGuards(ctx.functionGuards());
        }
        return String.format(
                "if (%s) {%n    %s%n}",
                String.join(" && ", conditions),
                addOffset(functionReturnStatement)
        );
    }

    private String processReturn(ConverterParser.FunctionResultContext ctx) {
        return String.format(
                "return %s;",
                new VariableMappingVisitor(
                        ((ConverterParser.FunctionCaseContext) ctx.getParent()).variables
                ).visitExpression(ctx.expression())
        );
    }

    private String processGuards(ConverterParser.FunctionGuardsContext ctx) {
        VariableMappingVisitor mappingVisitor =
                new VariableMappingVisitor(
                        ((ConverterParser.FunctionCaseContext) ctx.getParent()).variables
                );
        List<String> guards = new ArrayList<>();
        for (int i = 0; i < ctx.getChildCount(); i++) {
            ConverterParser.ConditionContext conditionContext = ctx.condition(i);
            guards.add(
                    String.format(
                            "if (%s) {%n    return %s;%n}",
                            mappingVisitor.visitBoolExpression(conditionContext.boolExpression()),
                            mappingVisitor.visitExpression(conditionContext.functionResult().expression())
                    )
            );
        }
        return String.join(" else ", guards);
    }

    private static String getTypeName(String type) {
        return PRIMITIVE_TYPES.getOrDefault(type, type);
    }

    private static String addOffset(String codeBlock) {
        return codeBlock.replace(LINE_SEPARATOR, LINE_OFFSET_SEPARATOR);
    }
}
