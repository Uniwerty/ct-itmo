package info.kgeorgiy.ja.ivchenkov.i18n;

import java.text.Collator;
import java.util.ArrayList;
import java.util.Comparator;

/**
 * The class to collect statistics for some textual category.
 *
 * @author Ivchenkov Dmitrii
 */
public class TextualStatistics extends Statistics<String> {
    private static final Comparator<String> LENGTH_COMPARATOR = Comparator.comparingInt(String::length);
    private double lengthSum;

    /**
     * Initializes a textual statistics entry.
     *
     * @param collator the {@link Collator} to compare strings according to the specified {@link java.util.Locale Locale}
     */
    public TextualStatistics(Collator collator) {
        super(collator);
    }

    /**
     * Gets the textual value with the maximum length.
     *
     * @return the maximum length value
     */
    public String getMaxLengthValue() {
        return getMaxByComparator(LENGTH_COMPARATOR);
    }

    /**
     * Gets the textual value with the minimum length.
     *
     * @return the minimum length value
     */
    public String getMinLengthValue() {
        return getMaxByComparator(LENGTH_COMPARATOR.reversed());
    }

    /**
     * Gets the average value length.
     *
     * @return the average value length
     */
    public Double getAverageLength() {
        return occurrences == 0 ? null : lengthSum / occurrences;
    }

    private String getMaxByComparator(Comparator<? super String> comparator) {
        return new ArrayList<>(values).stream().max(comparator).orElse(null);
    }

    @Override
    protected void updateSum(String value) {
        lengthSum += value.length();
    }
}
