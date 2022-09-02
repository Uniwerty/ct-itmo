package queue;

import java.util.function.Predicate;

public class LinkedQueue extends AbstractQueue {
    private Node head;
    private Node tail;

    private static class Node {
        private final Object element;
        private Node next;

        public Node(Object element, Node next) {
            this.element = element;
            this.next = next;
        }
    }

    @Override
    protected void enqueueImpl(Object element) {
        Node newTail = new Node(element, null);
        if (size == 0) {
            tail = newTail;
            head = tail;
        } else {
            tail.next = newTail;
            tail = newTail;
        }
    }

    @Override
    public Object element() {
        assert size > 0;
        return head.element;
    }

    @Override
    protected Object dequeueImpl() {
        final Object element = head.element;
        if (size == 0) {
            head = null;
            tail = null;
        } else {
            head = head.next;
        }
        return element;
    }

    @Override
    protected void clearImpl() {
        tail = null;
        head = null;
    }

    // :NOTE: вынесите общий код для indexIf и lastIndexIf
    // Fixed
    // indexIf и lastIndexIf теперь в абстрактном классе, а для реализаций
    // используется абстрактный метод indexes

    @Override
    protected int[] indexes(final Predicate<Object> predicate) {
        int firstIndex = -1;
        int lastIndex = -1;
        Node node = head;
        int i = 0;
        while (node != tail) {
            if (predicate.test(node.element)) {
                if (firstIndex == -1) {
                    firstIndex = i;
                }
                lastIndex = i;
            }
            node = node.next;
            i++;
        }
        if (node != null && predicate.test(node.element)) {
            if (firstIndex == -1) {
                firstIndex = i;
            }
            lastIndex = i;
        }
        // :NOTE: явный каст - это плохо
        return new int[]{firstIndex, lastIndex};
    }
}