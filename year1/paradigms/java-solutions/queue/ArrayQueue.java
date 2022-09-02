package queue;

import java.util.function.Predicate;

public class ArrayQueue extends AbstractQueue {
    private Object[] elements;
    private int head;
    private int tail;

    public ArrayQueue() {
        // :NOTE: почему 8? достаточно 1 или 2
        // Fixed
        elements = new Object[1];
        // head = 0;
        // tail = 0;
    }

    @Override
    protected void enqueueImpl(Object element) {
        this.ensureCapacity(size + 1);
        elements[tail] = element;
        tail = (tail + 1) % elements.length;
    }

    private void ensureCapacity(int requiredSize) {
        if (elements.length < requiredSize) {
            Object[] newElements = new Object[requiredSize * 2];
            reorganize(newElements);
        }
    }

    @Override
    public Object element() {
        assert size > 0;
        return elements[head];
    }

    @Override
    protected Object dequeueImpl() {
        Object element = elements[head];
        elements[head] = null;
        head = (head + 1) % elements.length;
        this.reduceCapacity();
        return element;
    }

    private void reduceCapacity() {
        if (size > 0 && size * 4 < elements.length) {
            Object[] newElements = new Object[size * 2];
            reorganize(newElements);
        }
    }

    private void reorganize(Object[] newElements) {
        if (head < tail) {
            System.arraycopy(elements, head, newElements, 0, tail - head);
        } else {
            System.arraycopy(elements, head, newElements, 0, elements.length - head);
            System.arraycopy(elements, 0, newElements, elements.length - head, tail);
        }
        head = 0;
        tail = size;
        elements = newElements;
    }

    @Override
    protected void clearImpl() {
        elements = new Object[1];
        head = 0;
        tail = 0;
    }

    // :NOTE: разветвление не нужно, подумайте, как можно без него
    // Fixed

    @Override
    protected int[] indexes(final Predicate<Object> predicate) {
        int firstIndex = -1;
        int lastIndex = -1;
        for (int i = head; i < tail + elements.length; i++) {
            if (predicate.test(elements[i % elements.length])) {
                if (firstIndex == -1) {
                    firstIndex = i - head;
                }
                lastIndex = (i - head) % elements.length;
            }
        }
        return new int[]{firstIndex, lastIndex};
    }

    public int indexOf(final Object element) {
        if (head < tail) {
            for (int i = head; i < tail; i++) {
                if (element.equals(elements[i])) {
                    return i - head;
                }
            }
        } else {
            for (int i = head; i < tail + elements.length; i++) {
                if (element.equals(elements[i % elements.length])) {
                    return i - head;
                }
            }
        }
        return -1;
    }

    public int lastIndexOf(final Object element) {
        int index = -1;
        if (head < tail) {
            for (int i = head; i < tail; i++) {
                if (element.equals(elements[i])) {
                    index = i - head;
                }
            }
        } else {
            for (int i = head; i < tail + elements.length; i++) {
                if (element.equals(elements[i % elements.length])) {
                    index = i - head;
                }
            }
        }
        return index;
    }
}