package converter;

import converter.exception.ConversionException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.stream.Stream;

public class ConverterTest {
    private static final Path CASES_DIRECTORY;

    static {
        try {
            CASES_DIRECTORY = Path.of(
                    Objects.requireNonNull(ConverterTest.class.getResource("/cases")).toURI()
            );
        } catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }

    @ParameterizedTest
    @MethodSource("correctCases")
    public void testCorrectCases(String caseName) throws IOException {
        Path casePath = CASES_DIRECTORY.resolve(Path.of("correct", caseName));
        Path inputFile = casePath.resolve(caseName);
        Path outputFile = casePath.resolve(caseName + "Result.java");
        Path sampleFile = casePath.resolve(caseName + ".java");
        FileConverter.convert(inputFile, outputFile);
        Assertions.assertEquals(
                Files.readString(sampleFile),
                Files.readString(outputFile)
        );
    }

    @ParameterizedTest
    @MethodSource("invalidCases")
    public void testInvalidCases(String caseName) {
        Path casePath = CASES_DIRECTORY.resolve(Path.of("invalid", caseName));
        Path inputFile = casePath.resolve(caseName);
        Path outputFile = casePath.resolve(caseName + "Result.java");
        Assertions.assertThrows(
                ConversionException.class,
                () -> FileConverter.convert(inputFile, outputFile)
        );
    }

    private static Stream<Arguments> correctCases() {
        return Stream.of(
                Arguments.of("NoArgumentsFunctions"),
                Arguments.of("UnaryFunctions"),
                Arguments.of("N-aryFunctions"),
                Arguments.of("NotFormattedFunctions"),
                Arguments.of("IntExpressionFunction"),
                Arguments.of("BoolExpressionFunction"),
                Arguments.of("ValueArgumentsFunctions"),
                Arguments.of("VariableArgumentsFunctions"),
                Arguments.of("MixedArgumentsFunctions"),
                Arguments.of("GuardsFunctions"),
                Arguments.of("RecursiveFunctions"),
                Arguments.of("InvokingFunctions")
        );
    }

    private static Stream<Arguments> invalidCases() {
        return Stream.of(
            Arguments.of("FunctionWithoutBody"),
            Arguments.of("FunctionNameMismatch"),
            Arguments.of("ReturnTypeMismatch"),
            Arguments.of("ArgumentsTypeMismatch"),
            Arguments.of("WrongArgumentsNumber"),
            Arguments.of("NonExistingFunction"),
            Arguments.of("MathExpressionTypeMismatch"),
            Arguments.of("BoolExpressionTypeMismatch"),
            Arguments.of("ApplicationArgumentsTypeMismatch")
        );
    }
}
