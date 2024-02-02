import java.util.*
import java.util.concurrent.*
import java.util.concurrent.atomic.*
import kotlin.random.Random

/**
 * @author Ivchenkov Dmitrii
 */
open class TreiberStackWithElimination<E> : Stack<E> {
    private val stack = TreiberStack<E>()

    private val eliminationArray = AtomicReferenceArray<Any?>(ELIMINATION_ARRAY_SIZE)

    override fun push(element: E) {
        if (tryPushElimination(element)) return
        stack.push(element)
    }

    protected open fun tryPushElimination(element: E): Boolean {
        val cellIndex = randomCellIndex()
        if (eliminationArray.compareAndSet(cellIndex, CELL_STATE_EMPTY, element)) {
            repeat(ELIMINATION_WAIT_CYCLES) {
                val current = eliminationArray.get(cellIndex)
                if (Objects.equals(current, CELL_STATE_RETRIEVED)) {
                    if (eliminationArray.compareAndSet(cellIndex, current, CELL_STATE_EMPTY)) {
                        return true
                    }
                }
            }
            val last = eliminationArray.getAndSet(cellIndex, CELL_STATE_EMPTY)
            return last == CELL_STATE_RETRIEVED
        }
        return false
    }

    override fun pop(): E? = tryPopElimination() ?: stack.pop()

    private fun tryPopElimination(): E? {
        val cellIndex = randomCellIndex()
        val cellValue = eliminationArray.get(cellIndex)
        if (!Objects.equals(cellValue, CELL_STATE_EMPTY) && !Objects.equals(cellValue, CELL_STATE_RETRIEVED)) {
            if (eliminationArray.compareAndSet(cellIndex, cellValue, CELL_STATE_RETRIEVED)) {
                return cellValue as E
            }
        }
        return null
    }

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(eliminationArray.length())

    companion object {
        private const val ELIMINATION_ARRAY_SIZE = 2 // Do not change!
        private const val ELIMINATION_WAIT_CYCLES = 1 // Do not change!

        // Initially, all cells are in EMPTY state.
        private val CELL_STATE_EMPTY = null

        // `tryPopElimination()` moves the cell state
        // to `RETRIEVED` if the cell contains element.
        private val CELL_STATE_RETRIEVED = Any()
    }
}
