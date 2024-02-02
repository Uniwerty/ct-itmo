import java.util.Objects
import java.util.concurrent.*
import java.util.concurrent.atomic.*

/**
 * @author Ivchenkov Dmitrii
 */
class FlatCombiningQueue<E> : Queue<E> {
    private val queue = ArrayDeque<E>() // sequential queue
    private val combinerLock = AtomicBoolean(false) // unlocked initially
    private val tasksForCombiner = AtomicReferenceArray<Any?>(TASKS_FOR_COMBINER_SIZE)

    override fun enqueue(element: E) {
        // TODO: Make this code thread-safe using the flat-combining technique.
        // TODO: 1.  Try to become a combiner by
        // TODO:     changing `combinerLock` from `false` (unlocked) to `true` (locked).
        // TODO: 2a. On success, apply this operation and help others by traversing
        // TODO:     `tasksForCombiner`, performing the announced operations, and
        // TODO:      updating the corresponding cells to `Result`.
        // TODO: 2b. If the lock is already acquired, announce this operation in
        // TODO:     `tasksForCombiner` by replacing a random cell state from
        // TODO:      `null` with the element. Wait until either the cell state
        // TODO:      updates to `Result` (do not forget to clean it in this case),
        // TODO:      or `combinerLock` becomes available to acquire.
        if (combinerLock.compareAndSet(false, true)) {
            queue.addLast(element)
            traverse()
            combinerLock.set(false)
        } else {
            while (true) {
                val i = randomCellIndex()
                if (tasksForCombiner.compareAndSet(i, null, element)) {
                    while (true) {
                        val result = tasksForCombiner.get(i)
                        if (!Objects.equals(result, element)) {
                            tasksForCombiner.set(i, null)
                            return
                        }
                        if (combinerLock.compareAndSet(false, true)) {
                            val current = tasksForCombiner.getAndSet(i, null)
                            if (!Objects.equals(current, element)) {
                                traverse()
                                combinerLock.set(false)
                                return
                            }
                            queue.addLast(element)
                            traverse()
                            combinerLock.set(false)
                            return
                        }
                    }
                } else if (combinerLock.compareAndSet(false, true)) {
                    queue.addLast(element)
                    traverse()
                    combinerLock.set(false)
                    return
                }
            }
        }
    }

    override fun dequeue(): E? {
        // TODO: Make this code thread-safe using the flat-combining technique.
        // TODO: 1.  Try to become a combiner by
        // TODO:     changing `combinerLock` from `false` (unlocked) to `true` (locked).
        // TODO: 2a. On success, apply this operation and help others by traversing
        // TODO:     `tasksForCombiner`, performing the announced operations, and
        // TODO:      updating the corresponding cells to `Result`.
        // TODO: 2b. If the lock is already acquired, announce this operation in
        // TODO:     `tasksForCombiner` by replacing a random cell state from
        // TODO:      `null` with `Dequeue`. Wait until either the cell state
        // TODO:      updates to `Result` (do not forget to clean it in this case),
        // TODO:      or `combinerLock` becomes available to acquire.
        if (combinerLock.compareAndSet(false, true)) {
            val element = queue.removeFirstOrNull()
            traverse()
            combinerLock.set(false)
            return element
        } else {
            while (true) {
                val i = randomCellIndex()
                if (tasksForCombiner.compareAndSet(i, null, Dequeue)) {
                    while (true) {
                        val result = tasksForCombiner.get(i)
                        if (!Objects.equals(result, Dequeue)) {
                            tasksForCombiner.set(i, null)
                            return (result as Result<E?>).value
                        }
                        if (combinerLock.compareAndSet(false, true)) {
                            val element = tasksForCombiner.getAndSet(i, null)
                            if (!Objects.equals(element, Dequeue)) {
                                traverse()
                                combinerLock.set(false)
                                return (element as Result<E?>).value
                            }
                            val first = queue.removeFirstOrNull()
                            traverse()
                            combinerLock.set(false)
                            return first
                        }
                    }
                } else if (combinerLock.compareAndSet(false, true)) {
                    val element = queue.removeFirstOrNull()
                    traverse()
                    combinerLock.set(false)
                    return element
                }
            }
        }
    }

    private fun traverse() {
        for (i in 0 until TASKS_FOR_COMBINER_SIZE) {
            val element = tasksForCombiner.get(i)
            if (Objects.equals(element, Dequeue)) {
                tasksForCombiner.set(i, Result(queue.removeFirstOrNull()))
            } else if (!Objects.equals(element, null) && element !is Result<*>) {
                queue.addLast(element as E)
                tasksForCombiner.set(i, Result(element as E))
            }
        }
    }

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(tasksForCombiner.length())
}

private const val TASKS_FOR_COMBINER_SIZE = 3 // Do not change this constant!

// TODO: Put this token in `tasksForCombiner` for dequeue().
// TODO: enqueue()-s should put the inserting element.
private object Dequeue

// TODO: Put the result wrapped with `Result` when the operation in `tasksForCombiner` is processed.
private class Result<V>(
    val value: V
)