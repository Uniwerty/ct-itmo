package info.kgeorgiy.ja.ivchenkov.i18n;

import java.util.Comparator;

/**
 * The abstract class to collect statistics for some numerical category.
 *
 * @param <T> the numerical category type
 * @author Ivchenkov Dmitrii
 */
public abstract class NumericalStatistics<T> extends Statistics<T> {
    protected NumericalStatistics(Comparator<? super T> comparator) {
        super(comparator);
    }

    /**
     * Gets the average value.
     *
     * @return the average value.
     */
    public abstract T getAverageValue();
}