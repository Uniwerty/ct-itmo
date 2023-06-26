package info.kgeorgiy.ja.ivchenkov.i18n;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.*;
import java.util.*;

/**
 * The class for collecting text files statistics and writing reports.
 *
 * @author Ivchenkov Dmitrii
 */
public class TextStatistics {
    private static final Charset CHARSET = StandardCharsets.UTF_8;
    private static final String BUNDLE_NAME = "info.kgeorgiy.ja.ivchenkov.i18n.ResourceBundle";
    private final TextualStatistics sentences;
    private final TextualStatistics words;
    private final NumberStatistics numbers;
    private final NumberStatistics money;
    private final DateStatistics dates;
    private final BreakIterator sentenceIterator;
    private final BreakIterator wordIterator;
    private final NumberFormat numberFormat;
    private final NumberFormat moneyFormat;
    private final DateFormat dateShortFormat;
    private final DateFormat dateMediumFormat;
    private final DateFormat dateLongFormat;
    private final DateFormat dateFullFormat;
    private final ResourceBundle bundle;

    /**
     * Initializes a text statistics instance to collect statistics.
     *
     * @param inputLocale  the input file's {@link Locale}
     * @param outputLocale the output file's {@link Locale}
     */
    public TextStatistics(Locale inputLocale, Locale outputLocale) {
        Collator collator = Collator.getInstance(inputLocale);
        sentences = new TextualStatistics(collator);
        words = new TextualStatistics(collator);
        numbers = new NumberStatistics();
        money = new NumberStatistics();
        dates = new DateStatistics();
        sentenceIterator = BreakIterator.getSentenceInstance(inputLocale);
        wordIterator = BreakIterator.getWordInstance(inputLocale);
        numberFormat = NumberFormat.getNumberInstance(inputLocale);
        moneyFormat = NumberFormat.getCurrencyInstance(inputLocale);
        dateShortFormat = DateFormat.getDateInstance(DateFormat.SHORT, inputLocale);
        dateMediumFormat = DateFormat.getDateInstance(DateFormat.MEDIUM, inputLocale);
        dateLongFormat = DateFormat.getDateInstance(DateFormat.LONG, inputLocale);
        dateFullFormat = DateFormat.getDateInstance(DateFormat.FULL, inputLocale);
        bundle = ResourceBundle.getBundle(BUNDLE_NAME, outputLocale);
    }

    /**
     * The main method of the class to collect statistics and write report.
     * <p>
     * Arguments: inputLocale outputLocale textFile reportFile.
     * <p>
     * inputLocale may be any locale, outputLocale should be either en_US or ru_RU.
     *
     * @param args the command line arguments
     */
    public static void main(String... args) {
        if (args == null || args.length != 4) {
            System.err.println("Invalid number of arguments! Please write: inputLocale outputLocale textFile reportFile");
            return;
        }
        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Invalid arguments given: arguments must be non-null values!");
            return;
        }
        Locale inputLocale;
        String[] parts = args[0].split("_");
        switch (parts.length) {
            case 1 -> inputLocale = new Locale(parts[0]);
            case 2 -> inputLocale = new Locale(parts[0], parts[1]);
            case 3 -> inputLocale = new Locale(parts[0], parts[1], parts[2]);
            default -> {
                System.err.println("Invalid input locale given!");
                return;
            }
        }
        Locale outputLocale;
        switch (args[1]) {
            case "en_US" -> outputLocale = new Locale("en", "US");
            case "ru_RU" -> outputLocale = new Locale("ru", "RU");
            default -> {
                System.err.println("The output locale must be en_US or ru_RU!");
                return;
            }
        }
        new TextStatistics(inputLocale, outputLocale).getStatistics(outputLocale, args[2], args[3]);
    }

    /**
     * Collects text statistics of the specified input file and writes the report to the specified output file.
     *
     * @param outputLocale the output file's {@link Locale}
     * @param inputFile    the input file to be analyzed
     * @param outputFile   the output file to be written
     */
    public void getStatistics(Locale outputLocale,
                              String inputFile,
                              String outputFile) {
        final String text;
        try {
            text = Files.readString(Path.of(inputFile), CHARSET);
        } catch (IOException e) {
            System.err.println("Some IO error occurred during reading input text: " + e.getMessage());
            return;
        }
        sentenceIterator.setText(text);
        int start = sentenceIterator.first();
        int end = sentenceIterator.next();
        while (end != BreakIterator.DONE) {
            String sentence = text.substring(start, end).trim();
            sentences.add(sentence);
            parseSentence(sentence);
            start = end;
            end = sentenceIterator.next();
        }
        try (BufferedWriter writer = Files.newBufferedWriter(Path.of(outputFile), CHARSET)) {
            writer.write(createReport(outputLocale, inputFile));
        } catch (IOException e) {
            System.err.println("Some IO error occurred during writing statistics: " + e.getMessage());
        }
    }

    private void parseSentence(String sentence) {
        wordIterator.setText(sentence);
        int start = wordIterator.first();
        int end = wordIterator.next();
        while (end != BreakIterator.DONE) {
            ParsePosition position = new ParsePosition(start);
            if (parseDateValue(sentence, position) ||
                    parseNumericalValue(moneyFormat, money, sentence, position) ||
                    parseNumericalValue(numberFormat, numbers, sentence, position)) {
                start = position.getIndex();
                end = wordIterator.following(start);
                continue;
            }
            String wordToken = sentence.substring(start, end);
            if (wordToken.chars().anyMatch(Character::isLetter)) {
                words.add(wordToken);
            }
            start = end;
            end = wordIterator.next();
        }
    }

    private boolean parseNumericalValue(NumberFormat numberFormat,
                                        NumberStatistics statistics,
                                        String sentence,
                                        ParsePosition position) {
        Number token = numberFormat.parse(sentence, position);
        if (token != null) {
            statistics.add(token.doubleValue());
            return true;
        }
        return false;
    }

    private boolean parseDateValue(String sentence, ParsePosition position) {
        return parseDateFormat(dateShortFormat, sentence, position) ||
                parseDateFormat(dateMediumFormat, sentence, position) ||
                parseDateFormat(dateLongFormat, sentence, position) ||
                parseDateFormat(dateFullFormat, sentence, position);
    }

    private boolean parseDateFormat(DateFormat dateFormat, String sentence, ParsePosition position) {
        Date token = dateFormat.parse(sentence, position);
        if (token != null) {
            dates.add(token);
            return true;
        }
        return false;
    }

    private String createReport(Locale outputLocale, String inputFile) throws IOException {
        NumberFormat numberOutput = NumberFormat.getNumberInstance(outputLocale);
        NumberFormat moneyOutput = NumberFormat.getCurrencyInstance(outputLocale);
        DateFormat dateOutput = DateFormat.getDateInstance(DateFormat.DEFAULT, outputLocale);
        final StringBuilder sb = new StringBuilder();
        writeSummary(sb, numberOutput, inputFile);
        writeStringStatistics(sb, numberOutput, "sentence", sentences);
        writeStringStatistics(sb, numberOutput, "word", words);
        writeNumericalStatistics(sb, numberOutput, numberOutput, "number", numbers);
        writeNumericalStatistics(sb, moneyOutput, numberOutput, "money", money);
        writeNumericalStatistics(sb, dateOutput, numberOutput, "date", dates);
        return sb.toString();
    }

    private void writeSummary(StringBuilder sb,
                              NumberFormat numberFormat,
                              String analyzedFile) {
        sb.append(MessageFormat.format(bundle.getString("title"), analyzedFile))
                .append(System.lineSeparator())
                .append(bundle.getString("summary"))
                .append(System.lineSeparator())
                .append(getSummaryStatistics(numberFormat, "sentence", sentences))
                .append(getSummaryStatistics(numberFormat, "word", words))
                .append(getSummaryStatistics(numberFormat, "number", numbers))
                .append(getSummaryStatistics(numberFormat, "money", money))
                .append(getSummaryStatistics(numberFormat, "date", dates));
    }

    private String getSummaryStatistics(NumberFormat numberFormat,
                                        String name,
                                        Statistics<?> statistics) {
        return String.format("\t%s %s%n",
                MessageFormat.format(bundle.getString(name + "Number"),
                        numberFormat.format(statistics.getCount())),
                MessageFormat.format(bundle.getString("uniqueNumber"),
                        numberFormat.format(statistics.getUniqueCount()))
        );
    }

    private void writeStringStatistics(StringBuilder sb,
                                       NumberFormat numberFormat,
                                       String name,
                                       TextualStatistics statistics) {
        if (statistics.getCount() > 0) {
            String minLengthValue = statistics.getMinLengthValue();
            String maxLengthValue = statistics.getMaxLengthValue();
            sb.append(bundle.getString(name + "Statistics"))
                    .append(System.lineSeparator())
                    .append(getSummaryStatistics(numberFormat, name, statistics))
                    .append('\t').append(MessageFormat.format(bundle.getString(name + "Min"),
                            statistics.getMinValue()))
                    .append(System.lineSeparator())
                    .append('\t').append(MessageFormat.format(bundle.getString(name + "Max"),
                            statistics.getMaxValue()))
                    .append(System.lineSeparator())
                    .append('\t').append(MessageFormat.format(bundle.getString(name + "MinLength"),
                            numberFormat.format(minLengthValue.length()), minLengthValue))
                    .append(System.lineSeparator())
                    .append('\t').append(MessageFormat.format(bundle.getString(name + "MaxLength"),
                            numberFormat.format(maxLengthValue.length()), maxLengthValue))
                    .append(System.lineSeparator())
                    .append('\t').append(MessageFormat.format(bundle.getString(name + "AverageLength"),
                            numberFormat.format(statistics.getAverageLength())))
                    .append(System.lineSeparator());
        }
    }

    private void writeNumericalStatistics(StringBuilder sb,
                                          Format numberFormat,
                                          NumberFormat countFormat,
                                          String name,
                                          NumericalStatistics<?> statistics) {
        if (statistics.getCount() > 0) {
            sb.append(bundle.getString(name + "Statistics"))
                    .append(System.lineSeparator())
                    .append(getSummaryStatistics(countFormat, name, statistics))
                    .append('\t').append(MessageFormat.format(bundle.getString(name + "Min"),
                            numberFormat.format(statistics.getMinValue())))
                    .append(System.lineSeparator())
                    .append('\t').append(MessageFormat.format(bundle.getString(name + "Max"),
                            numberFormat.format(statistics.getMaxValue())))
                    .append(System.lineSeparator())
                    .append('\t').append(MessageFormat.format(bundle.getString(name + "Average"),
                            numberFormat.format(statistics.getAverageValue())))
                    .append(System.lineSeparator());
        }
    }
}
