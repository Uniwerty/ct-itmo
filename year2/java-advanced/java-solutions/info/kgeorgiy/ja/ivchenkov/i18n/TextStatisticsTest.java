package info.kgeorgiy.ja.ivchenkov.i18n;

import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * The testing class.
 *
 * @author Ivchenkov Dmitrii
 */
public class TextStatisticsTest {
    private static final String SOURCE_DIRECTORY = "solutions/java-solutions/info/kgeorgiy/ja/ivchenkov/i18n/test-sources/";
    private static final Charset CHARSET = StandardCharsets.UTF_8;
    private static final String ENGLISH = "en_US";
    private static final String RUSSIAN = "ru_RU";
    private static final String GEORGIAN = "ge_GE";

    @Test
    public void emptyInputTest() throws IOException {
        runTest("emptyInput", ENGLISH, ENGLISH);
    }

    @Test
    public void numbersTest() throws IOException {
        runTest("numbersENtoEN", ENGLISH, ENGLISH);
        runTest("numbersRUtoEN", RUSSIAN, ENGLISH);
        runTest("numbersGEtoEN", GEORGIAN, ENGLISH);
    }

    @Test
    public void moneyTest() throws IOException {
        runTest("moneyENtoEN", ENGLISH, ENGLISH);
        runTest("moneyRUtoEN", RUSSIAN, ENGLISH);
        runTest("moneyGEtoEN", GEORGIAN, ENGLISH);
    }

    @Test
    public void datesTest() throws IOException {
        runTest("datesENtoEN", ENGLISH, ENGLISH);
        runTest("datesRUtoEN", RUSSIAN, ENGLISH);
        runTest("datesGEtoEN", GEORGIAN, ENGLISH);
    }

    @Test
    public void withoutNumbersTest() throws IOException {
        runTest("withoutNumbers", ENGLISH, ENGLISH);
    }

    @Test
    public void withoutMoneyTest() throws IOException {
        runTest("withoutMoney", ENGLISH, ENGLISH);
    }

    @Test
    public void withoutDatesTest() throws IOException {
        runTest("withoutDates", ENGLISH, ENGLISH);
    }

    @Test
    public void differentLanguagesTextsTest() throws IOException {
        runTest("englishTextToEN", ENGLISH, ENGLISH);
        runTest("englishTextToRU", ENGLISH, RUSSIAN);
        runTest("russianTextToEN", RUSSIAN, ENGLISH);
        runTest("russianTextToRU", RUSSIAN, RUSSIAN);
    }

    private void runTest(String testName, String inputLocale, String outputLocale) throws IOException {
        String input = SOURCE_DIRECTORY + testName + ".txt";
        String output = SOURCE_DIRECTORY + testName + "Result.txt";
        String correct = SOURCE_DIRECTORY + testName + "Correct.txt";
        TextStatistics.main(inputLocale, outputLocale, input, output);
        Assert.assertEquals(Files.readString(Path.of(correct), CHARSET), Files.readString(Path.of(output), CHARSET));
    }
}
