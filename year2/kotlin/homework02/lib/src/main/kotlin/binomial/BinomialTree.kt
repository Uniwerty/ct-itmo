package binomial

import java.lang.IllegalArgumentException

interface SelfMergeable<T> {
    operator fun plus(other: T): T
}

/*
 * BinomialTree - реализация биномиального дерева
 *
 * Вспомогательная структура для биномиальной кучи
 * https://en.wikipedia.org/wiki/Binomial_heap
 *
 * Запрещено использовать
 *
 *  - var
 *  - циклы
 *  - стандартные коллекции
 *
 * Детали внутренней реазации должны быть спрятаны
 * Создание - только через single() и plus()
 *
 * Дерево совсем без элементов не предусмотрено
 */

class BinomialTree<T : Comparable<T>> private constructor(val value: T, val children: FList<BinomialTree<T>>) :
    SelfMergeable<BinomialTree<T>> {
    // порядок дерева
    val order: Int = if (children == FList.Nil<T>()) 0 else children.first().order + 1

    companion object {
        fun <T : Comparable<T>> single(value: T): BinomialTree<T> = BinomialTree(value, FList.Nil())
    }

    /*
     * слияние деревьев
     * При попытке слить деревья разных порядков, нужно бросить IllegalArgumentException
     *
     * Требуемая сложность - O(1)
     */
    override fun plus(other: BinomialTree<T>): BinomialTree<T> {
        if (order == other.order) {
            return if (value < other.value) {
                BinomialTree(value, FList.Cons(other, children))
            } else {
                BinomialTree(other.value, FList.Cons(this, other.children))
            }
        } else {
            throw IllegalArgumentException("Binomial trees have different orders")
        }
    }
}
