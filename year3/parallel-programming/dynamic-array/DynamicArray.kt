package mpp.dynamicarray

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicReferenceArray

/**
 * @author Ivchenkov Dmitrii
 */
interface DynamicArray<E> {
    /**
     * Returns the element located in the cell [index],
     * or throws [IllegalArgumentException] if [index]
     * exceeds the [size] of this array.
     */
    fun get(index: Int): E

    /**
     * Puts the specified [element] into the cell [index],
     * or throws [IllegalArgumentException] if [index]
     * exceeds the [size] of this array.
     */
    fun put(index: Int, element: E)

    /**
     * Adds the specified [element] to this array
     * increasing its [size].
     */
    fun pushBack(element: E)

    /**
     * Returns the current size of this array,
     * it increases with [pushBack] invocations.
     */
    val size: Int
}

class DynamicArrayImpl<E> : DynamicArray<E> {
    private val core = AtomicReference(Core<E>(INITIAL_CAPACITY))

    override fun get(index: Int): E {
        if (index >= size) throw IllegalArgumentException()
        return core.get().get(index)
    }

    override fun put(index: Int, element: E) {
        if (index >= size) throw IllegalArgumentException()
        core.get().put(index, element)
    }

    override fun pushBack(element: E) {
        var curCore = core.get()
        while (true) {
            val curSize = curCore.size.get()
            if (curSize < curCore.capacity) {
                if (curCore.array.compareAndSet(curSize, null, element)) {
                    curCore.size.compareAndSet(curSize, curSize + 1)
                    return
                } else {
                    curCore.size.compareAndSet(curSize, curSize + 1)
                }
            } else {
                curCore = resize(curCore)
            }
        }
    }

    @Suppress("UNCHECKED_CAST")
    private fun resize(curCore: Core<E>): Core<E> {
        val newCore = Core<E>(curCore.capacity * 2)
        newCore.size.set(curCore.capacity)
        curCore.next.compareAndSet(null, newCore)
        val nextCore = curCore.next.get() ?: curCore
        var index = 0
        while (index < curCore.capacity) {
            val element = curCore.array[index]
            if (element is Moved) {
                // pass
            } else if (element == null) {
                if (!nextCore.array.compareAndSet(index, null, Propagated(curCore))) {
                    continue
                }
            } else if (element is Propagated<*>) {
                if (!nextCore.array.compareAndSet(index, null, Propagated(curCore))) {
                    continue
                }
            } else if (element is Fixed<*>) {
                val value = element.value as E
                nextCore.array.compareAndSet(index, null, value)
                curCore.array.compareAndSet(index, element, Moved)
            } else {
                curCore.array.compareAndSet(index, element, Fixed(element))
                continue
            }
            index++
        }
        core.compareAndSet(curCore, nextCore)
        return nextCore
    }

    override val size: Int get() = core.get().size.get()

    inner class Core<E>(val capacity: Int) {
        val array = AtomicReferenceArray<Any>(capacity)
        val size = AtomicInteger(0)
        val next = AtomicReference<Core<E>>(null)

        @Suppress("UNCHECKED_CAST")
        fun get(index: Int): E {
            while (true) {
                val element = array[index]
                if (element is Moved) {
                    return next.get().get(index)
                } else if (element is Fixed<*>) {
                    return element.value as E
                } else if (element is Propagated<*>) {
                    return element.origin.get(index) as E
                } else if (element != null) {
                    return element as E
                }
            }
        }

        @Suppress("UNCHECKED_CAST")
        fun put(index: Int, element: E) {
            while (true) {
                val current = array[index]
                if (current is Moved) {
                    return next.get().put(index, element)
                } else if (current is Fixed<*>) {
                    next.get().put(index, current.value as E)
                    array.compareAndSet(index, current, Moved)
                } else {
                    if (array.compareAndSet(index, current, element)) {
                        return
                    }
                }
            }
        }
    }

    class Fixed<E>(val value: E)
    class Propagated<E>(val origin: DynamicArrayImpl<E>.Core<E>)
    object Moved
}

private const val INITIAL_CAPACITY = 1 // DO NOT CHANGE ME
