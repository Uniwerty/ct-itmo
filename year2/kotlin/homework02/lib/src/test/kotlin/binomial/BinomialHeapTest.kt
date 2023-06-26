package binomial

import kotlin.test.Test
import kotlin.test.assertEquals

internal class BinomialHeapTest {
    @Test
    fun simple() {
        val heap = BinomialHeap.single(234)
        assertEquals(234, heap.top())
    }

    @Test
    fun plusSimple() {
        val simple1 = BinomialHeap.single(234)
        val simple2 = BinomialHeap.single(345)
        val heap1 = simple1 + simple2
        val heap2 = simple2 + simple1

        assertEquals(234, heap1.top())
        assertEquals(234, heap2.top())
    }

    @Test
    fun insertions() {
        val values = listOf(342, 32143, 23, 15, 43, 434, 32, 4324, 4324, 4234, 12, 345, 2123, 324, 41)

        values.fold(BinomialHeap.single(1000)) { acc, current ->
            val topPrev = acc.top()
            val result = acc + current
            val topNext = result.top()
            if (current < topPrev) {
                assertEquals(current, topNext)
            } else {
                assertEquals(topPrev, topNext)
            }
            result
        }
    }

    @Test
    fun insertionsAscending() {
        val values = 0 .. 100000

        values.fold(BinomialHeap.single(1000)) { acc, current ->
            val result = acc + current
            assertEquals(0, result.top())
            result
        }
    }

    @Test
    fun insertionsDescending() {
        val values = 100000 downTo 0

        values.fold(BinomialHeap.single(1000000)) { acc, current ->
            val result = acc + current
            assertEquals(current, result.top())
            result
        }
    }

    @Test
    fun insertionsRandom() {
        val values = (0 .. 100000).map { Math.random() * 100000 }

        values.fold(BinomialHeap.single(2000000.0)) { acc, current ->
            val topPrev = acc.top()
            val result = acc + current
            val topNext = result.top()
            if (current < topPrev) {
                assertEquals(current, topNext)
            } else {
                assertEquals(topPrev, topNext)
            }
            result
        }
    }

    @Test
    fun dropSimple() {
        val simple1 = BinomialHeap.single(234)
        val simple2 = BinomialHeap.single(345)
        val heap1 = simple1 + simple2
        val heap2 = simple2 + simple1

        assertEquals(345, heap1.drop().top())
        assertEquals(345, heap2.drop().top())
    }

    @Test
    fun dropAscending() {
        val values = 0 .. 100000

        val heapBig = values.fold(BinomialHeap.single(987654321)) { acc, current ->
            acc + current
        }

        val heapSmall = values.fold(heapBig) { acc, current ->
            assertEquals(current, acc.top())
            acc.drop()
        }

        assertEquals(987654321, heapSmall.top())
    }

    @Test
    fun dropDescending() {
        val values = 100000 downTo 0

        val heapBig = values.fold(BinomialHeap.single(987654321)) { acc, current ->
            acc + current
        }

        val heapSmall = values.reversed().fold(heapBig) { acc, current ->
            assertEquals(current, acc.top())
            acc.drop()
        }

        assertEquals(987654321, heapSmall.top())
    }

    @Test
    fun dropRandom() {
        val values = (0 .. 100000).map { Math.random() * 100000 }

        val heapBig = values.fold(BinomialHeap.single(987654321.0)) { acc, current ->
            acc + current
        }

        val heapSmall = values.sorted().fold(heapBig) { acc, current ->
            assertEquals(current, acc.top())
            acc.drop()
        }

        assertEquals(987654321.0, heapSmall.top())

    }

    @Test
    fun mergeAscending() {
        val values = 0 .. 200000
        val values1 = 0 .. 100000
        val heapBig1 = values1.fold(BinomialHeap.single(987654321)) { acc, current ->
            acc + current
        }
        val values2 = 100001 .. 200000
        val heapBig2 = values2.fold(BinomialHeap.single(987654321)) { acc, current ->
            acc + current
        }

        val heapSmall = values.fold(heapBig1 + heapBig2) { acc, current ->
            assertEquals(current, acc.top())
            acc.drop()
        }

        assertEquals(987654321, heapSmall.top())
        assertEquals(987654321, heapSmall.drop().top())
    }

    @Test
    fun mergeDescending() {
        val values = 200000 downTo 0
        val values1 = 100000 downTo 0
        val values2 = 200000 downTo 100001

        val heapBig1 = values1.fold(BinomialHeap.single(987654321)) { acc, current ->
            acc + current
        }
        val heapBig2 = values2.fold(BinomialHeap.single(987654321)) { acc, current ->
            acc + current
        }

        val heapSmall = values.reversed().fold(heapBig1 + heapBig2) { acc, current ->
            assertEquals(current, acc.top())
            acc.drop()
        }

        assertEquals(987654321, heapSmall.top())
        assertEquals(987654321, heapSmall.drop().top())
    }

    @Test
    fun mergeRandom() {
        val values1 = (0 .. 100000).map { Math.random() * 100000 }
        val values2 = (0 .. 100000).map { Math.random() * 100000 }

        val heapBig1 = values1.fold(BinomialHeap.single(987654321.0)) { acc, current ->
            acc + current
        }
        val heapBig2 = values2.fold(BinomialHeap.single(987654321.0)) { acc, current ->
            acc + current
        }

        val heapSmall = (values1 + values2).sorted().fold(heapBig1 + heapBig2) { acc, current ->
            assertEquals(current, acc.top())
            acc.drop()
        }

        assertEquals(987654321.0, heapSmall.top())
        assertEquals(987654321.0, heapSmall.drop().top())
    }
}