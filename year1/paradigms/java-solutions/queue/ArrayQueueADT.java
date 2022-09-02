package queue;

// Model: a[head]..a[tail-1]
// Invariant: ∀ i = head..tail-1: a[i] != null && head <= tail
public class ArrayQueueADT {
    private Object[] elements = new Object[8];
    private int head;
    private int tail;
    private int size;

    // Pred: true
    // Post: Invariant
    public static ArrayQueueADT create() {
        final ArrayQueueADT queue = new ArrayQueueADT();
        queue.elements = new Object[8];
        // queue.head = 0;
        // queue.tail = 0;
        return queue;
    }

    // Let immutable(l, r): ∀ i = l..r-1: a'[i] == a[i]

    // Pred: element != null && queue != null
    // Post: a[tail] == element && tail' == tail + 1 && head' == head && immutable(head, tail)
    public static void enqueue(final ArrayQueueADT queue, final Object element) {
        assert element != null;
        ensureCapacity(queue, queue.size + 1);
        queue.elements[queue.tail] = element;
        queue.tail = (queue.tail + 1) % queue.elements.length;
        queue.size++;
    }

    private static void ensureCapacity(final ArrayQueueADT queue, int requiredSize) {
        if (queue.elements.length < requiredSize) {
            Object[] newElements = new Object[requiredSize * 2];
            if (queue.head < queue.tail) {
                System.arraycopy(queue.elements, queue.head, newElements, 0, queue.tail - queue.head);
            } else {
                System.arraycopy(queue.elements, queue.head, newElements, 0, queue.elements.length - queue.head);
                System.arraycopy(queue.elements, 0, newElements, queue.elements.length - queue.head, queue.tail);
            }
            queue.head = 0;
            queue.tail = queue.size;
            queue.elements = newElements;
        }
    }

    // Pred: head != tail && queue != null
    // Post: R == a[head] && head' == head && tail' == tail && immutable(head, tail)
    public static Object element(final ArrayQueueADT queue) {
        assert queue.size > 0;
        return queue.elements[queue.head];
    }

    // Pred: head != tail && queue != null
    // Post: R == a[head] && head' == head + 1 && tail' == tail && immutable(head + 1, tail)
    public static Object dequeue(final ArrayQueueADT queue) {
        assert queue.size > 0;
        Object element = queue.elements[queue.head];
        queue.elements[queue.head] = null;
        queue.head = (queue.head + 1) % queue.elements.length;
        queue.size--;
        return element;
    }

    // Pred: queue != null
    // Post: R == tail - head && head' == head && tail' == tail && immutable(head, tail)
    public static int size(final ArrayQueueADT queue) {
        return queue.size;
    }

    // Pred: queue != null
    // Post: R == (head == tail) && head' == head && tail' == tail && immutable(head, tail)
    public static boolean isEmpty(final ArrayQueueADT queue) {
        return queue.size == 0;
    }

    // Pred: queue != null
    // Post: Invariant && head' == tail'
    public static void clear(final ArrayQueueADT queue) {
        queue.elements = new Object[8];
        queue.head = 0;
        queue.tail = 0;
        queue.size = 0;
    }

    // Pred: element != null && queue != null
    // Post: element.equals(a[R]) && ∀ i < R: !element.equals(a[i]) && head' == head && tail' == tail && immutable(head, tail)
    public static int indexOf(final ArrayQueueADT queue, final Object element) {
        if (queue.head < queue.tail) {
            for (int i = queue.head; i < queue.tail; i++) {
                if (element.equals(queue.elements[i])) {
                    return i - queue.head;
                }
            }
        } else {
            for (int i = queue.head; i < queue.tail + queue.elements.length; i++) {
                if (element.equals(queue.elements[i % queue.elements.length])) {
                    return i - queue.head;
                }
            }
        }
        return -1;
    }

    // Pred: element != null && queue != null
    // Post: element.equals(a[R]) && ∀ i > R: !element.equals(a[i]) && head' == head && tail' == tail && immutable(head, tail)
    public static int lastIndexOf(final ArrayQueueADT queue, final Object element) {
        int index = -1;
        if (queue.head < queue.tail) {
            for (int i = queue.head; i < queue.tail; i++) {
                if (element.equals(queue.elements[i])) {
                    index = i - queue.head;
                }
            }
        } else {
            for (int i = queue.head; i < queue.tail + queue.elements.length; i++) {
                if (element.equals(queue.elements[i % queue.elements.length])) {
                    index = i - queue.head;
                }
            }
        }
        return index;
    }
}
