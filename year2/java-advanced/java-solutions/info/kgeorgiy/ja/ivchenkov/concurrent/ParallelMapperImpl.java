package info.kgeorgiy.ja.ivchenkov.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;

/**
 * The class for parallel mapping lists.
 *
 * @author Ivchenkov Dmitrii
 */
public class ParallelMapperImpl implements ParallelMapper {
    private final List<Thread> threadsList;
    private final Queue<Runnable> queue;

    /**
     * Constructs the mapper with the specified threads number.
     *
     * @param threads the number of working {@link Thread threads}
     */
    public ParallelMapperImpl(int threads) {
        queue = new ArrayDeque<>();
        threadsList = new ArrayList<>(threads);
        for (int i = 0; i < threads; i++) {
            threadsList.add(new Thread(() -> {
                try {
                    while (!Thread.interrupted()) {
                        try {
                            doTask();
                        } catch (RuntimeException ignored) {
                        }
                    }
                } catch (InterruptedException ignored) {
                }
            }));
        }
        threadsList.forEach(Thread::start);
    }

    /**
     * Maps the specified list with the provided function.
     *
     * @param f    the {@link Function} to apply
     * @param args the {@link List} to map
     * @param <T>  the input list values type
     * @param <R>  the result list values type
     * @return the result list of mapping {@code args} with {@code f}
     * @throws InterruptedException if any thread has interrupted one of the executed threads
     */
    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args) throws InterruptedException {
        final List<R> results = new ArrayList<>(Collections.nCopies(args.size(), null));
        final Counter counter = new Counter(args.size());
        final List<Runnable> tasks = new ArrayList<>(args.size());
        final RuntimeException[] exceptions = new RuntimeException[1];
        for (int i = 0; i < args.size(); i++) {
            final int index = i;
            tasks.add(() -> {
                try {
                    results.set(index, f.apply(args.get(index)));
                    synchronized (counter) {
                        counter.increment();
                        if (counter.isDone()) {
                            counter.notify();
                        }
                    }
                } catch (RuntimeException exception) {
                    if (exceptions[0] == null) {
                        exceptions[0] = exception;
                    } else {
                        exceptions[0].addSuppressed(exception);
                    }
                }
            });
        }

        synchronized (queue) {
            queue.addAll(tasks);
            queue.notifyAll();
        }

        synchronized (counter) {
            while (!counter.isDone()) {
                counter.wait();
            }
        }

        if (exceptions[0] != null) {
            throw exceptions[0];
        }

        return results;
    }

    /**
     * Closes the mapper and interrupts all its threads.
     */
    @Override
    public void close() {
        threadsList.forEach(Thread::interrupt);
        try {
            Util.join(threadsList);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    private void doTask() throws InterruptedException {
        final Runnable task;
        synchronized (queue) {
            while (queue.isEmpty()) {
                queue.wait();
            }
            task = queue.poll();
            queue.notifyAll();
        }
        task.run();
    }

    private static class Counter {
        private int count;
        private final int limit;

        public Counter(int limit) {
            this.count = 0;
            this.limit = limit;
        }

        public boolean isDone() {
            return count >= limit;
        }

        public void increment() {
            count++;
        }
    }
}