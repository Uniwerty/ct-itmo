package info.kgeorgiy.ja.ivchenkov.i18n;

import java.math.BigInteger;
import java.util.Comparator;
import java.util.Date;

/**
 * The class to collect date statistics.
 *
 * @author Ivchenkov Dmitrii
 */
public class DateStatistics extends NumericalStatistics<Date> {
    private BigInteger sum;

    /**
     * Initializes a date statistics entry.
     */
    public DateStatistics() {
        super(Comparator.naturalOrder());
        sum = BigInteger.ZERO;
    }

    /**
     * Gets the average date.
     *
     * @return the average date
     */
    @Override
    public Date getAverageValue() {
        return occurrences == 0 ? null : new Date(sum.divide(BigInteger.valueOf(occurrences)).longValue());
    }

    @Override
    protected void updateSum(Date value) {
        sum = sum.add(BigInteger.valueOf(value.getTime()));
    }
}
