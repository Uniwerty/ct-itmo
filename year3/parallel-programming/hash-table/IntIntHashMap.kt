import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicReferenceArray

/*
  :WARNING:
  This is an incorrect implementation
 */

/**
 * Int-to-Int hash map with open addressing and linear probes.
 */
class IntIntHashMap {
    private var core = AtomicReference(Core(INITIAL_CAPACITY))

    /**
     * Returns value for the corresponding key or zero if this key is not present.
     *
     * @param key a positive key.
     * @return value for the corresponding or zero if this key is not present.
     * @throws IllegalArgumentException if key is not positive.
     */
    operator fun get(key: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        return toValue(core.get().getInternal(key))
    }

    /**
     * Changes value for the corresponding key and returns old value or zero if key was not present.
     *
     * @param key   a positive key.
     * @param value a positive value.
     * @return old value or zero if this key was not present.
     * @throws IllegalArgumentException if key or value are not positive, or value is equal to
     * [Integer.MAX_VALUE] which is reserved.
     */
    fun put(key: Int, value: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        require(isValue(value)) { "Invalid value: $value" }
        var curCore = core.get()
        while (true) {
            val oldValue = curCore.putInternal(key, value)
            if (oldValue != NEEDS_REHASH) return oldValue
            curCore = rehash(curCore)
        }
    }

    /**
     * Removes value for the corresponding key and returns old value or zero if key was not present.
     *
     * @param key a positive key.
     * @return old value or zero if this key was not present.
     * @throws IllegalArgumentException if key is not positive.
     */
    fun remove(key: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        var curCore = core.get()
        while (true) {
            val oldValue = curCore.removeInternal(key)
            if (oldValue != NEEDS_REHASH) return oldValue
            curCore = rehash(curCore)
        }
    }

    private fun rehash(curCore: Core): Core {
        val newCore = Core(curCore.keys.length() * 2)
        curCore.next.compareAndSet(null, newCore)
        val nextCore = curCore.next.get() ?: curCore
        var index = 0
        while (index < curCore.keys.length()) {
            val key = curCore.keys[index]
            val value = curCore.values[index]
            if (value is Moved) {
                // pass
            } else if (value == NULL_VALUE || value is Deleted) {
                if (!curCore.values.compareAndSet(index, value, Moved)) {
                    continue
                }
            } else if (value is Fixed) {
                nextCore.keys.compareAndSet(nextCore.index(key), NULL_KEY, key)
                nextCore.values.compareAndSet(nextCore.index(key), NULL_VALUE, value.value)
                curCore.values.compareAndSet(index, value, Moved)
            } else {
                curCore.values.compareAndSet(index, value, Fixed(value as Int))
                continue
            }
            index++
        }
        core.compareAndSet(curCore, nextCore)
        return nextCore
    }

    private inner class Core(capacity: Int) {
        val keys = AtomicReferenceArray<Int>(capacity)
        val values = AtomicReferenceArray<Any>(capacity)
        val next = AtomicReference<Core>(null)
        val shift: Int

        init {
            val mask = capacity - 1
            assert(mask > 0 && mask and capacity == 0) { "Capacity must be power of 2: $capacity" }
            shift = 32 - Integer.bitCount(mask)
            for (i in 0 until capacity) {
                keys.set(i, NULL_KEY)
                values.set(i, NULL_VALUE)
            }
        }

        fun getInternal(key: Int): Int {
            var index = index(key)
            var probes = 0
            while (probes < MAX_PROBES) {
                val curKey = keys[index]
                if (curKey == key) {
                    val result = values[index]
                    if (result is Moved) {
                        return next.get().getInternal(key)
                    } else if (result is Fixed) {
                        return result.value
                    } else if (result is Deleted) {
                        // pass
                        return NULL_VALUE
                    } else {
                        return result as Int
                    }
                } else if (curKey == NULL_KEY) {
                    return NULL_VALUE
                }
                if (index == 0) index = keys.length()
                index--
                probes++
            }
            return NULL_VALUE
        }

        fun putInternal(key: Int, value: Int): Int {
            var index = index(key)
            var probes = 0
            while (probes < MAX_PROBES) {
                while (true) {
                    val curKey = keys[index]
                    if (curKey == key) {
                        val curValue = values[index]
                        if (curValue is Moved) {
                            return next.get().putInternal(key, value)
                        } else if (curValue is Fixed) {
                            next.get().putInternal(key, curValue.value)
                            values.compareAndSet(index, curValue, Moved)
                        } else {
                            if (values.compareAndSet(index, curValue, value)) {
                                return if (curValue is Deleted) 0 else curValue as Int
                            }
                        }
                    } else if (curKey == NULL_KEY) {
                        val curValue = values[index]
                        if (keys.compareAndSet(index, curKey, key)) {
                            if (values.compareAndSet(index, curValue, value)) {
                                return curValue as Int
                            }
                        }
                    } else {
                        break
                    }
                }
                if (index == 0) index = keys.length()
                index--
                probes++
            }
            return NEEDS_REHASH
        }

        fun removeInternal(key: Int): Int {
            var index = index(key)
            var probes = 0
            while (probes < MAX_PROBES) {
                while (true) {
                    val curKey = keys[index]
                    if (curKey == key) {
                        val curValue = values[index]
                        if (curValue is Moved) {
                            return next.get().removeInternal(key)
                        } else if (curValue is Fixed) {
                            next.get().removeInternal(key)
                            values.compareAndSet(index, curValue, Moved)
                        } else if (curValue is Deleted) {
                            break
                        } else {
                            if (values.compareAndSet(index, curValue, Deleted)) {
                                return curValue as Int
                            }
                        }
                    } else if (curKey == NULL_KEY) {
                        return NULL_VALUE
                    } else {
                        break
                    }
                }
                if (index == 0) index = keys.length()
                index--
                probes++
            }
            return NEEDS_REHASH
        }

        /**
         * Returns an initial index in map to look for a given key.
         */
        fun index(key: Int): Int = (key * MAGIC ushr shift)
    }
}

object Moved
object Deleted
class Fixed(val value: Int)

private const val MAGIC = -0x61c88647 // golden ratio
private const val INITIAL_CAPACITY = 2 // !!! DO NOT CHANGE INITIAL CAPACITY !!!
private const val MAX_PROBES = 8 // max number of probes to find an item
private const val NULL_KEY = 0 // missing key (initial value)
private const val NULL_VALUE = 0 // missing value (initial value)
private const val DEL_VALUE = Int.MAX_VALUE // mark for removed value
private const val NEEDS_REHASH = -1 // returned by `putInternal` to indicate that rehash is needed
private const val MOVED_VALUE = Int.MIN_VALUE

// Checks is the value is in the range of allowed values
private fun isValue(value: Int): Boolean = value in (1 until DEL_VALUE)

// Converts internal value to the public results of the methods
private fun toValue(value: Int): Int = if (isValue(value)) value else 0