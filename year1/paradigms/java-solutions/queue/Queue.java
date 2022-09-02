package queue;

import java.util.function.Predicate;

// Model: a[head]..a[tail-1]
// Invariant: ∀ i = head..tail-1: a[i] != null && head <= tail
public interface Queue {
    // Let immutable(l, r): ∀ i = l..r-1: a'[i] == a[i]

    // Pred: element != null && this != null
    // Post: a[tail] == element && tail' == tail + 1 && head' == head && immutable(head, tail)
    void enqueue(Object element);

    // Pred: head != tail && this != null
    // Post: R == a[head] && head' == head && tail' == tail && immutable(head, tail)
    Object element();

    // Pred: head != tail && this != null
    // Post: R == a[head] && head' == head + 1 && tail' == tail && immutable(head + 1, tail)
    Object dequeue();

    // Pred: this != null
    // Post: R == tail - head && head' == head && tail' == tail && immutable(head, tail)
    int size();

    // Pred: this != null
    // Post: R == (head == tail) && head' == head && tail' == tail && immutable(head, tail)
    boolean isEmpty();

    // Pred: this != null
    // Post: Invariant && head' == tail'
    void clear();

    // Pred: this != null
    // Post: predicate.test(a[R]) && ∀ i < R: !predicate.test(a[i]) && head' == head && tail' == tail && immutable(head, tail)
    int indexIf(Predicate<Object> predicate);

    // Pred: this != null
    // Post: predicate.test(a[R]) && ∀ i > R: !predicate.test(a[i]) && head' == head && tail' == tail && immutable(head, tail)
    int lastIndexIf(Predicate<Object> predicate);
}
