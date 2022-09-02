package queue;

import java.util.function.Predicate;

public abstract class AbstractQueue implements Queue {
    protected int size;

    public void enqueue(final Object element) {
        assert element != null;
        enqueueImpl(element);
        size++;
    }

    protected abstract void enqueueImpl(Object element);

    public abstract Object element();

    public Object dequeue() {
        assert size > 0;
        size--;
        return dequeueImpl();
    }

    protected abstract Object dequeueImpl();

    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public void clear() {
        clearImpl();
        size = 0;
    }

    protected abstract void clearImpl();

    public int indexIf(Predicate<Object> predicate) {
        return indexes(predicate)[0];
    }

    public int lastIndexIf(Predicate<Object> predicate) {
        return indexes(predicate)[1];
    }

    protected abstract int[] indexes(Predicate<Object> predicate);
}
