import java.util.concurrent.atomic.*

/**
 * @author Ivchenkov Dmitrii
 *
 * TODO: Copy the code from `FAABasedQueueSimplified`
 * TODO: and implement the infinite array on a linked list
 * TODO: of fixed-size `Segment`s.
 */
class FAABasedQueue<E> : Queue<E> {
    private val head: AtomicReference<Segment>
    private val tail: AtomicReference<Segment>
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)

    init {
        val initial = Segment(0)
        head = AtomicReference(initial)
        tail = AtomicReference(initial)
    }

    override fun enqueue(element: E) {
        while (true) {
            val curTail = tail.get()
            val i = enqIdx.getAndIncrement()
            val segment = findSegment(curTail, i / SEGMENT_SIZE)
            if (segment.cells.compareAndSet((i % SEGMENT_SIZE).toInt(), null, element)) {
                return
            }
        }
    }

    override fun dequeue(): E? {
        while (true) {
            if (deqIdx.get() >= enqIdx.get()) {
                return null
            }
            val curHead = head.get()
            val i = deqIdx.getAndIncrement()
            val segment = findSegment(curHead, i / SEGMENT_SIZE)
            if (segment.cells.compareAndSet((i % SEGMENT_SIZE).toInt(), null, POISONED)) {
                continue
            }
            return segment.cells.getAndSet((i % SEGMENT_SIZE).toInt(), null) as E?
        }
    }

    private fun findSegment(start: Segment, id: Long): Segment {
        var current = start
        for (i in start.id until id) {
            val next = current.next.get()
            current = if (next == null) {
                val new = Segment(i + 1)
                current.next.compareAndSet(null, new)
                current.next.get()!!
            } else {
                next
            }
        }
        return current
    }
}

private class Segment(val id: Long) {
    val next = AtomicReference<Segment?>(null)
    val cells = AtomicReferenceArray<Any?>(SEGMENT_SIZE)
}

// DO NOT CHANGE THIS CONSTANT
private const val SEGMENT_SIZE = 2
private val POISONED = Any()
