package binomial

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlin.test.fail

internal class BinomialTreeTest {

    @Test
    fun getOrder() {
        assertEquals(0, BinomialTree.single(134).order)
        assertEquals(0, BinomialTree.single("hello").order)
    }

    @Test
    fun getMerge0() {
        val bt1 = BinomialTree.single(123)
        val bt2 = BinomialTree.single(234)
        val merged12 = bt1 + bt2
        val merged21 = bt2 + bt1
        val merged11 = bt1 + bt1
        val merged22 = bt2 + bt2

        listOf(merged11, merged12, merged21, merged22).forEach {
                assertEquals(1, it.order)
        }

        listOf(merged11, merged12, merged21).forEach {
            assertEquals(123, it.value)
        }

        listOf(merged22).forEach {
            assertEquals(234, it.value)
        }
    }

    @Test
    fun getMerge1() {
        val bt1 = BinomialTree.single(123) + BinomialTree.single(234)
        val bt2 = BinomialTree.single(12) + BinomialTree.single(345)
        val merged12 = bt1 + bt2
        val merged21 = bt2 + bt1
        val merged11 = bt1 + bt1
        val merged22 = bt2 + bt2

        listOf(merged11, merged12, merged21, merged22).forEach {
            assertEquals(2, it.order)
        }

        listOf(merged22, merged12, merged21).forEach {
            assertEquals(12, it.value)
        }

        listOf(merged11).forEach {
            assertEquals(123, it.value)
        }
    }

    @Test
    fun badMerge1() {
        val bt1 = BinomialTree.single(123)
        val bt2 = BinomialTree.single(234)
        val bt3 = bt1 + bt2

        try {
            bt1 + bt3
            fail("failed to throw exception")
        } catch (e: IllegalArgumentException) {
        }
    }

    @Test
    fun badMerge2() {
        val bt1 = BinomialTree.single(123)
        val bt2 = BinomialTree.single(234)
        val bt3 = bt1 + bt2

        try {
            bt3 + bt2
            fail("failed to throw exception")
        } catch (e: IllegalArgumentException) {
        }
    }
}