package info.kgeorgiy.ja.ivchenkov.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

/**
 * The class for parallel lists processing.
 *
 * @author Ivchenkov Dmitrii
 */
public class IterativeParallelism implements ScalarIP {
    private final ParallelMapper mapper;

    /**
     * Constructs the default instance.
     */
    public IterativeParallelism() {
        this.mapper = null;
    }

    /**
     * Constructs the instance with the specified mapper.
     * @param mapper the {@link ParallelMapper} to process lists.
     */
    public IterativeParallelism(ParallelMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * Returns the first maximum element of the specified list according to the provided comparator.
     *
     * @param threads    the number of concurrent threads.
     * @param values     the values {@link List} to get maximum of.
     * @param comparator the value {@link Comparator}.
     * @param <T>        the type of values.
     * @return the first maximum element of {@code values} list according to {@code comparator}.
     * @throws InterruptedException if any thread has interrupted one of the executed threads.
     */
    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        Function<Stream<? extends T>, T> getMaximum = stream -> stream.max(comparator).orElseThrow();
        return getResult(threads, values, getMaximum, getMaximum);
    }

    /**
     * Returns the first minimum element of the specified list according to the provided comparator.
     *
     * @param threads    the number of concurrent threads.
     * @param values     the values {@link List} to get minimum of.
     * @param comparator the value {@link Comparator}.
     * @param <T>        the type of values.
     * @return the first minimum element of {@code values} list according to {@code comparator}.
     * @throws InterruptedException if any thread has interrupted one of the executed threads.
     */
    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, values, comparator.reversed());
    }

    /**
     * Checks whether all elements of the specified list satisfy the provided predicate.
     *
     * @param threads   the number of concurrent threads.
     * @param values    the values {@link List} to test.
     * @param predicate the test {@link Predicate}.
     * @param <T>       the type of values.
     * @return {@code true} if all elements of {@code values} list satisfy the provided {@code predicate}, otherwise {@code false}.
     * @throws InterruptedException if any thread has interrupted one of the executed threads.
     */
    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return getResult(
                threads,
                values,
                stream -> stream.allMatch(predicate),
                stream -> stream.allMatch(value -> value)
        );
    }

    /**
     * Checks whether any elements of the specified list satisfy the provided predicate.
     *
     * @param threads   the number of concurrent threads.
     * @param values    the values {@link List} to test.
     * @param predicate the test {@link Predicate}.
     * @param <T>       the type of elements.
     * @return {@code true} if any elements of {@code values} list satisfy the provided {@code predicate}, otherwise {@code false}.
     * @throws InterruptedException if any thread has interrupted one of the executed threads.
     */
    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return !all(threads, values, predicate.negate());
    }

    /**
     * Counts the number of elements of the specified list that satisfy the provided predicate.
     *
     * @param threads   the number of concurrent threads.
     * @param values    the values {@link List} to test.
     * @param predicate the test {@link Predicate}.
     * @param <T>       the type of elements.
     * @return the number of elements of {@code values} list that satisfy {@code predicate}.
     * @throws InterruptedException if any thread has interrupted one of the executed threads.
     */
    @Override
    public <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return getResult(
                threads,
                values,
                stream -> stream.filter(predicate).mapToInt(e -> 1).sum(),
                stream -> stream.mapToInt(value -> (int) value).reduce(Integer::sum).orElse(0)
        );
    }

    private <T, R> R getResult(int threads,
                               List<? extends T> values,
                               Function<Stream<? extends T>, R> evaluateFunction,
                               Function<Stream<? extends R>, R> resultFunction) throws InterruptedException {
        if (threads == 0) {
            throw new IllegalArgumentException("The number of threads must be positive!");
        }

        final int parts = Math.max(Math.min(threads, values.size()), 1);
        final List<List<? extends T>> listPartition = getListPartition(values, parts);
        final List<R> partialResults;
        if (mapper == null) {
            partialResults = new ArrayList<>(parts);
            for (int i = 0; i < parts; i++) {
                partialResults.add(null);
            }
            final List<Thread> threadsList = new ArrayList<>(parts);
            for (int i = 0; i < listPartition.size(); i++) {
                final int index = i;
                Thread thread = new Thread(() -> partialResults.set(index, evaluateFunction.apply(listPartition.get(index).stream())));
                thread.start();
                threadsList.add(thread);
            }
            Util.join(threadsList);
        } else {
            partialResults = mapper.map((list) -> evaluateFunction.apply(list.stream()), listPartition);
        }
        return resultFunction.apply(partialResults.stream());
    }

    private static <T> List<List<? extends T>> getListPartition(List<? extends T> list, int parts) {
        List<List<? extends T>> listParts = new ArrayList<>(parts);
        int partSize = list.size() / parts;
        int partRemainder = list.size() % parts;
        int right = 0;
        for (int i = 0; i < parts; i++) {
            int left = right;
            right += partSize + (partRemainder > 0 ? 1 : 0);
            partRemainder--;
            listParts.add(list.subList(left, right));
        }
        return listParts;
    }


}