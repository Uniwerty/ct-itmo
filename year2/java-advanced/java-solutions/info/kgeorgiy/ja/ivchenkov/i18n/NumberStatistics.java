package info.kgeorgiy.ja.ivchenkov.i18n;

import java.util.Comparator;

/**
 * The class to collect number statistics.
 *
 * @author Ivchenkov Dmitrii
 */
public class NumberStatistics extends NumericalStatistics<Double> {
    private double sum;

    /**
     * Initializes a number statistics entry.
     */
    public NumberStatistics() {
        super(Comparator.naturalOrder());
    }

    /**
     * Gets the average number.
     *
     * @return the average number.
     */
    @Override
    public Double getAverageValue() {
        return occurrences == 0 ? null : sum / occurrences;
    }

    @Override
    protected void updateSum(Double value) {
        sum += value;
    }
}
