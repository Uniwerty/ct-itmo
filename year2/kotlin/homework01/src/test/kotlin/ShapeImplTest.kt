import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.function.Executable;
import kotlin.test.Test

internal class ShapeImplTest {
    @Test
    fun testNDim() {
        val cases = listOf(
            Pair(DefaultShape(1), 1),
            Pair(DefaultShape(10), 1),
            Pair(DefaultShape(1, 1), 2),
            Pair(DefaultShape(1, 5), 2),
            Pair(DefaultShape(5, 1), 2),
            Pair(DefaultShape(5, 3), 2),
            Pair(DefaultShape(1, 1, 1), 3),
            Pair(DefaultShape(1, 5, 3), 3),
            Pair(DefaultShape(5, 1, 3), 3),
            Pair(DefaultShape(5, 3, 2), 3),
        )

        cases.forEach {
            assertEquals(it.second, it.first.ndim)
        }
    }

    @Test
    fun testSize() {
        val cases = listOf(
            Pair(DefaultShape(1), 1),
            Pair(DefaultShape(10), 10),
            Pair(DefaultShape(1, 1), 1),
            Pair(DefaultShape(1, 5), 5),
            Pair(DefaultShape(5, 1), 5),
            Pair(DefaultShape(5, 3), 15),
            Pair(DefaultShape(1, 1, 1), 1),
            Pair(DefaultShape(1, 5, 3), 15),
            Pair(DefaultShape(5, 1, 3), 15),
            Pair(DefaultShape(5, 3, 2), 30),
        )

        cases.forEach {
            assertEquals(it.second, it.first.size)
        }
    }

    @Test
    fun testEmpty() {
        assertThrows(ShapeArgumentException.EmptyShapeException::class.java) {
            DefaultShape()
        }
    }

    @Test
    fun testNonPositive() {
        assertThrows(ShapeArgumentException.NonPositiveDimensionException::class.java) {
            DefaultShape(0)
        }
        assertThrows(ShapeArgumentException.NonPositiveDimensionException::class.java) {
            DefaultShape(0, 1)
        }
        assertThrows(ShapeArgumentException.NonPositiveDimensionException::class.java) {
            DefaultShape(5, 0)
        }
        assertThrows(ShapeArgumentException.NonPositiveDimensionException::class.java) {
            DefaultShape(-1, 10)
        }
        assertThrows(ShapeArgumentException.NonPositiveDimensionException::class.java) {
            DefaultShape(10, -1)
        }
        assertThrows(ShapeArgumentException.NonPositiveDimensionException::class.java) {
            DefaultShape(0, 0)
        }
        assertThrows(ShapeArgumentException.NonPositiveDimensionException::class.java) {
            DefaultShape(1, 2, 3, 0)
        }
    }
}