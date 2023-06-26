package binomial

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

internal class FListTest {
    @Test
    fun testEmpty() {
        assertTrue { FList.nil.isEmpty }
        assertFalse { FList.Cons("hello", FList.nil).isEmpty }
        assertFalse { FList.Cons("hello", FList.Cons("world", FList.nil)).isEmpty }
    }

    @Test
    fun testSize() {
        assertEquals(0, FList.nil.size)
        assertEquals(1, FList.Cons("hello", FList.nil).size)
        assertEquals(2, FList.Cons("hello", FList.Cons("world", FList.nil)).size)
    }

    @Test
    fun testToList() {
        assertEquals(emptyList(), FList.nil.toList())
        assertEquals(listOf("hello"), FList.Cons("hello", FList.nil).toList())
        assertEquals(
            listOf("hello", "world"),
            FList.Cons("hello", FList.Cons("world", FList.nil)).toList()
        )
    }

    @Test
    fun testFList() {
        assertTrue { flistOf<String>().isEmpty }
        assertTrue { flistOf<Int>().isEmpty }
        assertFalse { flistOf("hello").isEmpty }
        assertFalse { flistOf("hello", "world").isEmpty }

        assertEquals(0, flistOf<String>().size)
        assertEquals(1, flistOf("hello").size)
        assertEquals(2, flistOf("hello", "world").size)
    }

    @Test
    fun testMap() {
        assertEquals(FList.nil(), FList.nil<String>().map { it.length })
        assertEquals(FList.Cons(5, FList.nil()), FList.Cons("hello", FList.nil()).map { it.length })
        assertEquals(
            FList.Cons(5, FList.Cons(6, FList.nil())),
            FList.Cons("hello", FList.Cons("people", FList.nil())).map { it.length }
        )
    }

    @Test
    fun testFilter() {
        assertEquals(FList.nil(), FList.nil<String>().filter { it.length > 0 && it[0] == 'h' })
        assertEquals(
            FList.Cons("hello", FList.nil()),
            FList.Cons("hello", FList.nil()).filter { it.length > 0 && it[0] == 'h' }
        )
        assertEquals(FList.nil(), FList.Cons("hello", FList.nil()).filter { it.length > 0 && it[0] != 'h' })
        assertEquals(
            FList.Cons("hello", FList.nil()),
            FList.Cons("hello", FList.Cons("people", FList.nil())).filter { it.length > 0 && it[0] == 'h' }
        )
        assertEquals(
            FList.Cons("people", FList.nil()),
            FList.Cons("hello", FList.Cons("people", FList.nil())).filter { it.length > 0 && it[0] != 'h' }
        )
    }

    @Test
    fun testFold() {
        val lengthSum = { acc: Int, current: String ->
            current.length + acc
        }
        assertEquals(1, flistOf<String>().fold(1, lengthSum))
        assertEquals(5, flistOf("hello").fold(0, lengthSum))
        assertEquals(11, flistOf("hello", "people").fold(0, lengthSum))
    }

    @Test
    fun testReverse() {
        assertEquals(flistOf(), flistOf<String>().reverse())
        assertEquals(flistOf("hello"), flistOf("hello").reverse())
        assertEquals(flistOf("people", "hello"), flistOf("hello", "people").reverse())
    }
}
