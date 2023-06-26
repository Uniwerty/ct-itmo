package info.kgeorgiy.ja.ivchenkov.i18n;

import java.util.Comparator;
import java.util.TreeSet;

/**
 * The abstract class to collect statistics for some text category.
 *
 * @param <T> the text category type
 * @author Ivchenkov Dmitrii
 */
public abstract class Statistics<T> {
    protected int occurrences;
    protected final TreeSet<T> values;

    protected Statistics(Comparator<? super T> comparator) {
        this.values = new TreeSet<>(comparator);
    }

    /**
     * Adds a value to statistics.
     *
     * @param value the value to be added
     */
    public void add(T value) {
        occurrences++;
        values.add(value);
        updateSum(value);
    }

    protected abstract void updateSum(T value);

    /**
     * Gets the count of all values.
     *
     * @return the count of statistic values
     */
    public int getCount() {
        return occurrences;
    }

    /**
     * Gets the count of unique values.
     *
     * @return the count of unique statistics values
     */
    public int getUniqueCount() {
        return values.size();
    }

    /**
     * Gets the minimum value of the statistics.
     *
     * @return the minimum value of the statistics.
     */
    public T getMinValue() {
        return values.isEmpty() ? null : values.first();
    }

    /**
     * Gets the maximum value of the statistics.
     *
     * @return the maximum value of the statistics.
     */
    public T getMaxValue() {
        return values.isEmpty() ? null : values.last();
    }
}
