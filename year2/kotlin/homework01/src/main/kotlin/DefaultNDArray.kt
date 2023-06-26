interface NDArray : SizeAware, DimensionAware {
    /*
     * Получаем значение по индексу point
     *
     * Если размерность point не равна размерности NDArray
     * бросаем IllegalPointDimensionException
     *
     * Если позиция по любой из размерностей некорректна с точки зрения
     * размерности NDArray, бросаем IllegalPointCoordinateException
     */
    fun at(point: Point): Int

    /*
     * Устанавливаем значение по индексу point
     *
     * Если размерность point не равна размерности NDArray
     * бросаем IllegalPointDimensionException
     *
     * Если позиция по любой из размерностей некорректна с точки зрения
     * размерности NDArray, бросаем IllegalPointCoordinateException
     */
    fun set(point: Point, value: Int)

    /*
     * Копируем текущий NDArray
     *
     */
    fun copy(): NDArray

    /*
     * Создаем view для текущего NDArray
     *
     * Ожидается, что будет создан новая реализация  интерфейса.
     * Но она не должна быть видна в коде, использующем эту библиотеку как внешний артефакт
     *
     * Должна быть возможность делать view над view.
     *
     * In-place-изменения над view любого порядка видна в оригнале и во всех view
     *
     * Проблемы thread-safety игнорируем
     */
    fun view(): NDArray

    /*
     * In-place сложение
     *
     * Размерность other либо идентична текущей, либо на 1 меньше
     * Если она на 1 меньше, то по всем позициям, кроме "лишней", она должна совпадать
     *
     * Если размерности совпадают, то делаем поэлементное сложение
     *
     * Если размерность other на 1 меньше, то для каждой позиции последней размерности мы
     * делаем поэлементное сложение
     *
     * Например, если размерность this - (10, 3), а размерность other - (10), то мы для три раза прибавим
     * other к каждому срезу последней размерности
     *
     * Аналогично, если размерность this - (10, 3, 5), а размерность other - (10, 5), то мы для пять раз прибавим
     * other к каждому срезу последней размерности
     */
    fun add(other: NDArray)

    /*
     * Умножение матриц. Immutable-операция. Возвращаем NDArray
     *
     * Требования к размерности - как для умножения матриц.
     *
     * this - обязательно двумерна
     *
     * other - может быть двумерной, с подходящей размерностью, равной 1 или просто вектором
     *
     * Возвращаем новую матрицу (NDArray размерности 2)
     *
     */
    fun dot(other: NDArray): NDArray
}

/*
 * Базовая реализация NDArray
 *
 * Конструкторы должны быть недоступны клиенту
 *
 * Инициализация - через factory-методы ones(shape: Shape), zeros(shape: Shape) и метод copy
 */
class DefaultNDArray private constructor(private val shape: Shape, private val data: IntArray) : NDArray {
    override val size = shape.size
    override val ndim = shape.ndim
    private val dimensions = IntArray(ndim) { i -> shape.dim(i) }

    override fun dim(i: Int): Int = dimensions[i]

    companion object {
        private fun createWithValue(shape: Shape, initValue: Int): NDArray = DefaultNDArray(shape, IntArray(shape.size) { initValue })
        fun zeros(shape: Shape): NDArray = createWithValue(shape, 0)
        fun ones(shape: Shape): NDArray = createWithValue(shape, 1)
    }

    override fun at(point: Point): Int {
        return data[pointToIndex(point)]
    }

    override fun set(point: Point, value: Int) {
        data[pointToIndex(point)] = value
    }

    override fun copy(): NDArray {
        return DefaultNDArray(shape, data.copyOf())
    }

    override fun view(): NDArray {
        return NDArrayView(this)
    }

    override fun add(other: NDArray) {
        when (ndim) {
            other.ndim -> {
                for (i in dimensions.indices) {
                    if (dim(i) != other.dim(i)) {
                        throw NDArrayException.NDArrayDimensionMismatchException()
                    }
                }
                for (i in 0 until size) {
                    data[i] += (other as DefaultNDArray).data[i]
                }
            }
            other.ndim + 1 -> {
                var missingDimension = -1
                var missingWas = false
                var thisIndex = 0
                var otherIndex = 0
                while (otherIndex < other.ndim) {
                    if (dim(thisIndex) != other.dim(otherIndex)) {
                        if (missingWas) {
                            throw NDArrayException.NDArrayDimensionMismatchException()
                        }
                        missingDimension = thisIndex
                        missingWas = true
                        thisIndex++
                    } else {
                        thisIndex++
                        otherIndex++
                    }
                }
                if (missingDimension == -1) {
                    missingDimension = ndim - 1
                }
                val layerSize = size / dim(missingDimension)
                for (d in 0 until dim(missingDimension)) {
                    for (i in 0 until layerSize) {
                        data[i * dim(missingDimension) + d] += (other as DefaultNDArray).data[i]
                    }
                }
            }
            else -> throw NDArrayException.NDArrayDimensionMismatchException()
        }
    }

    override fun dot(other: NDArray): NDArray {
        if (ndim == 2 && other.ndim == 2 && dim(1) == other.dim(0)) {
            val ndArray = zeros(DefaultShape(dim(0), other.dim(1))) as DefaultNDArray
            for (i in 0 until dim(0)) {
                for (j in 0 until other.dim(1)) {
                    var value = 0
                    for (k in 0 until dim(1)) {
                        value += data[i * dim(1) + k] * (other as DefaultNDArray).data[k * other.dim(1) + j]
                    }
                    ndArray.data[i * other.dim(1) + j] = value
                }
            }
            return ndArray
        } else if (ndim == 2 && other.ndim == 1 && dim(1) == other.dim(0)) {
            val ndArray = zeros(DefaultShape(dim(0), 1)) as DefaultNDArray
            for (i in 0 until dim(0)) {
                var value = 0
                for (k in 0 until dim(1)) {
                    value += data[i * dim(1) + k] * (other as DefaultNDArray).data[k]
                }
                ndArray.data[i] = value
            }
            return ndArray
        } else {
            throw NDArrayException.NDArrayDimensionMismatchException()
        }
    }

    private fun pointToIndex(point: Point): Int {
        if (ndim != point.ndim) {
            throw NDArrayException.IllegalPointDimensionException()
        }
        var layerSize = size
        var index = 0
//        for (i in ndim - 1 downTo 0) {
        for (i in 0 until ndim) {
            layerSize /= dim(i)
            if (point.dim(i) >= dim(i)) {
                throw NDArrayException.IllegalPointCoordinateException()
            }
            index += layerSize * point.dim(i)
        }
        return index
    }
}

internal class NDArrayView(private val ndArray: NDArray) : NDArray {
    override val size = ndArray.size
    override val ndim = ndArray.ndim
    override fun dim(i: Int): Int = ndArray.dim(i)

    override fun at(point: Point): Int {
        return ndArray.at(point)
    }

    override fun set(point: Point, value: Int) {
        ndArray.set(point, value)
    }

    override fun copy(): NDArray {
        return NDArrayView(ndArray)
    }

    override fun view(): NDArray {
        return NDArrayView(this.ndArray)
    }

    override fun add(other: NDArray) {
        ndArray.add(other)
    }

    override fun dot(other: NDArray): NDArray {
        return ndArray.dot(other)
    }
}

sealed class NDArrayException(reason: String = "") : Exception(reason) {
    class IllegalPointCoordinateException : NDArrayException("Point dimension is invalid")
    class IllegalPointDimensionException : NDArrayException("Number of point dimensions is invalid")
    class NDArrayDimensionMismatchException : NDArrayException("Dimensions do not match")
}