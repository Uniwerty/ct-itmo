package info.kgeorgiy.ja.ivchenkov.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements SortedSet<E> {
    private final List<E> elements;
    private final Comparator<? super E> comparator;

    public ArraySet(Collection<E> collection, Comparator<? super E> comparator) {
        List<E> list = new ArrayList<>(collection);
        this.comparator = comparator;
        list.sort(comparator);
        this.elements = removeDuplicates(list);
    }

    public ArraySet(Collection<E> collection) {
        this(collection, null);
    }

    public ArraySet() {
        this(List.of(), null);
    }

    protected ArraySet(List<E> list, Comparator<? super E> comparator) {
        this.comparator = comparator;
        this.elements = list;
    }

    @Override
    public Iterator<E> iterator() {
        return elements.iterator();
    }

    @Override
    public int size() {
        return elements.size();
    }

    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        if (comparator != null && comparator.compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException("Invalid subSet requested: fromElement is greater than toElement");
        }
        return new ArraySet<>(elements.subList(indexOf(fromElement), indexOf(toElement)), comparator);
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        return new ArraySet<>(elements.subList(0, indexOf(toElement)), comparator);
    }

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        return new ArraySet<>(elements.subList(indexOf(fromElement), elements.size()), comparator);
    }

    @Override
    public E first() {
        if (elements.isEmpty()) {
            throw new NoSuchElementException();
        }
        return elements.get(0);
    }

    @Override
    public E last() {
        if (elements.isEmpty()) {
            throw new NoSuchElementException();
        }
        return elements.get(elements.size() - 1);
    }

    @Override
    public boolean contains(Object o) {
        @SuppressWarnings("unchecked")
        E e = (E) o;
        int index = indexOf(e);
        if (index >= elements.size()) {
            return false;
        }
        return Collections.binarySearch(elements, e, comparator) == index;
    }

    private List<E> removeDuplicates(List<E> list) {
        List<E> result = new ArrayList<>();
        for (E e : list) {
            if (Collections.binarySearch(result, e, comparator) < 0) {
                result.add(e);
            }
        }
        return result;
    }

    private int indexOf(E element) {
        int index = Collections.binarySearch(elements, element, comparator);
        return index < 0 ? -(index + 1) : index;
    }
}