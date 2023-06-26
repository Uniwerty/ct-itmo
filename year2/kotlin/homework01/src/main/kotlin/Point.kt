interface Point : DimensionAware

/**
 * Реализация Point по умолчаению
 *
 * Должны работать вызовы DefaultPoint(10), DefaultPoint(12, 3), DefaultPoint(12, 3, 12, 4, 56)
 * с любым количество параметров
 *
 * Сама коллекция параметров недоступна, доступ - через методы интерфейса
 */
class DefaultPoint(private vararg val indexes: Int) : Point {
    override val ndim = indexes.size
    override fun dim(i: Int): Int = indexes[i]

    override fun toString(): String {
        return indexes.contentToString()
    }
}