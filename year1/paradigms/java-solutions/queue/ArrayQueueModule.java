package queue;

// Model: a[head]..a[tail-1]
// Invariant: ∀ i = head..tail-1: a[i] != null && head <= tail
public class ArrayQueueModule {
    private static Object[] elements = new Object[8];
    private static int head;
    private static int tail;
    private static int size;

    // Let immutable(l, r): ∀ i = l..r-1: a'[i] == a[i]

    // Pred: element != null
    // Post: a[tail] == element && tail' == tail + 1 && head' == head && immutable(head, tail)
    public static void enqueue(final Object element) {
        assert element != null;
        ensureCapacity(size + 1);
        elements[tail] = element;
        tail = (tail + 1) % elements.length;
        size++;
    }

    private static void ensureCapacity(int requiredSize) {
        if (elements.length < requiredSize) {
            Object[] newElements = new Object[requiredSize * 2];
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
    }

    // Pred: head != tail
    // Post: R == a[head] && head' == head && tail' == tail && immutable(head, tail)
    public static Object element() {
        assert size > 0;
        return elements[head];
    }

    // Pred: head != tail
    // Post: R == a[head] && head' == head + 1 && tail' == tail && immutable(head + 1, tail)
    public static Object dequeue() {
        assert size > 0;
        Object element = elements[head];
        elements[head] = null;
        head = (head + 1) % elements.length;
        size--;
        return element;
    }

    // Pred: true
    // Post: R == tail - head && head' == head && tail' == tail && immutable(head, tail)
    public static int size() {
        return size;
    }

    // Pred: true
    // Post: R == (head == tail) && head' == head && tail' == tail && immutable(head, tail)
    public static boolean isEmpty() {
        return size == 0;
    }

    // Pred: true
    // Post: Invariant && head' == tail'
    public static void clear() {
        elements = new Object[8];
        head = 0;
        tail = 0;
        size = 0;
    }

    // Pred: element != null
    // Post: a[R].equals(element) && ∀ i < R: !a[i].equals(element) && head' == head && tail' == tail && immutable(head, tail)
    public static int indexOf(final Object element) {
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

    // Pred: element != null
    // Post: a[R].equals(element) && ∀ i > R: !a[i].equals(element) && head' == head && tail' == tail && immutable(head, tail)
    public static int lastIndexOf(final Object element) {
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
