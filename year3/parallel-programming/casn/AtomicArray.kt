import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicReferenceArray

/**
 * @author Ivchenkov Dmitrii
 */
class AtomicArray<E : Any>(size: Int, initialValue: E) {
    private val a = AtomicReferenceArray<Any>(size)

    init {
        for (i in 0 until size) a[i] = initialValue
    }

    fun get(index: Int): E {
        while (true) {
            when (val cur = a[index]) {
                is Descriptor -> cur.complete()
                else -> return cur as E
            }
        }
    }

    fun set(index: Int, value: E) {
        while (true) {
            when (val cur = a[index]) {
                is Descriptor -> cur.complete()
                else -> if (a.compareAndSet(index, cur, value)) {
                    return
                }
            }
        }
    }

    fun cas(index: Int, expected: Any, update: Any): Boolean {
        while (true) {
            when (val cur = a[index]) {
                is Descriptor -> cur.complete()
                expected -> if (a.compareAndSet(index, cur, update)) {
                    return true
                }

                else -> return false
            }
        }
    }

    fun cas2(
        index1: Int, expected1: E, update1: E,
        index2: Int, expected2: E, update2: E
    ): Boolean {
        if (index1 == index2) {
            return if (expected1 == expected2) {
                cas(index1, expected1, update2)
            } else {
                false
            }
        }
        val descriptor = if (index1 < index2) {
            CasnDescriptor(
                index1, expected1, update1,
                index2, expected2, update2
            )
        } else {
            CasnDescriptor(
                index2, expected2, update2,
                index1, expected1, update1
            )
        }
        return if (index1 < index2) {
            if (cas(index1, expected1, descriptor)) {
                descriptor.complete()
                descriptor.outcome.get() == true
            } else {
                false
            }
        } else {
            if (cas(index2, expected2, descriptor)) {
                descriptor.complete()
                descriptor.outcome.get() == true
            } else {
                false
            }
        }
    }

    inner class DcssDescriptor(
        val index1: Int, val expected1: Any, val update1: Any,
        val flag: AtomicReference<Boolean?>, val expected2: Boolean?
    ) : Descriptor {
        val outcome = AtomicReference<Boolean?>(null)

        override fun complete() {
            outcome.compareAndSet(null, flag.get() == expected2)
            val result = outcome.get()!!
            if (result) {
                a.compareAndSet(index1, this, update1)
            } else {
                a.compareAndSet(index1, this, expected1)
            }
        }
    }

    inner class CasnDescriptor<E : Any>(
        val index1: Int, val expected1: E, val update1: E,
        val index2: Int, val expected2: E, val update2: E
    ) : Descriptor {
        val outcome: AtomicReference<Boolean?> = AtomicReference(null)

        fun dcss(): Boolean {
            val descriptor = DcssDescriptor(index2, expected2, this, outcome, null)
            return if (a.get(index2) == this) {
                return true
            } else if (cas(index2, expected2, descriptor)) {
                descriptor.complete()
                descriptor.outcome.get() == true
            } else {
                false
            }
        }

        override fun complete() {
            if (dcss()) {
                outcome.compareAndSet(null, true)
            } else {
                outcome.compareAndSet(null, a.get(index2) == this)
            }
            val result = outcome.get()!!
            if (result) {
                a.compareAndSet(index1, this, update1)
                a.compareAndSet(index2, this, update2)
            } else {
                a.compareAndSet(index1, this, expected1)
                a.compareAndSet(index2, this, expected2)
            }
        }
    }

    private interface Descriptor {
        fun complete()
    }
}