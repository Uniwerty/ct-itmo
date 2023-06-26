package binomial

/*
 * BinomialHeap - реализация биномиальной кучи
 *
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
 * Куча совсем без элементов не предусмотрена
 *
 * Операции
 *
 * plus с кучей
 * plus с элементом
 * top - взятие минимального элемента
 * drop - удаление минимального элемента
 */
class BinomialHeap<T : Comparable<T>> private constructor(private val trees: FList<BinomialTree<T>?>) :
    SelfMergeable<BinomialHeap<T>> {
    companion object {
        fun <T : Comparable<T>> single(value: T): BinomialHeap<T> = BinomialHeap(flistOf(BinomialTree.single(value)))
    }

    /*
     * слияние куч
     *
     * Требуемая сложность - O(log(n))
     */
    override fun plus(other: BinomialHeap<T>): BinomialHeap<T> = BinomialHeap(plusImpl(trees, other.trees, null))

    /*
     * добавление элемента
     * 
     * Требуемая сложность - O(log(n))
     */
    operator fun plus(elem: T): BinomialHeap<T> = plus(single(elem))

    /*
     * минимальный элемент
     *
     * Требуемая сложность - O(log(n))
     */
    fun top(): T {
        val validTrees = trees.filter { it != null }
        return validTrees.fold(validTrees.first()!!.value) { acc, tree -> minOf(acc, tree!!.value) }
    }

    /*
     * удаление элемента
     *
     * Требуемая сложность - O(log(n))
     */
    fun drop(): BinomialHeap<T> {
        val validTrees = trees.filter { it != null } as FList<BinomialTree<T>>
        val minTree = findMinTree(validTrees, validTrees.first(), validTrees.first().value)
        val heap = BinomialHeap(deleteTreeByKey(trees, minTree.value))
        val deletedHeap = BinomialHeap(minTree.children.reverse() as FList<BinomialTree<T>?>)
        return heap + deletedHeap
    }

    // Возвращает результат слияния двух списков деревьев
    private fun plusImpl(
        first: FList<BinomialTree<T>?>,
        second: FList<BinomialTree<T>?>,
        carry: BinomialTree<T>?
    ): FList<BinomialTree<T>?> {
        if (first is FList.Nil && second is FList.Nil) {
            if (carry == null) {
                return FList.Nil()
            } else {
                return FList.Cons(carry, FList.Nil())
            }
        }
        val result = plusWithCarry(
            if (first is FList.Cons) first.head else null,
            if (second is FList.Cons) second.head else null,
            carry
        )
        return FList.Cons(
            result.first,
            plusImpl(
                if (first is FList.Cons) first.tail else FList.Nil(),
                if (second is FList.Cons) second.tail else FList.Nil(),
                result.second
            )
        )
    }

    // Возвращает результат слияния двух деревьев и предыдущего результата
    private fun plusWithCarry(
        first: BinomialTree<T>?,
        second: BinomialTree<T>?,
        carry: BinomialTree<T>?
    ): Pair<BinomialTree<T>?, BinomialTree<T>?> {
        return if (first == null) {
            if (second == null) {
                Pair(carry, null)
            } else if (carry == null) {
                Pair(second, null)
            } else {
                Pair(null, second + carry)
            }
        } else if (second == null) {
            if (carry == null) {
                Pair(first, null)
            } else {
                Pair(null, first + carry)
            }
        } else Pair(carry, first + second)
    }

    // Находит в списке дерево с минимальным элементом
    private fun findMinTree(trees: FList<BinomialTree<T>>, minTree: BinomialTree<T>, minValue: T): BinomialTree<T> {
        return if (trees is FList.Nil) {
            minTree
        } else if ((trees as FList.Cons).head.value < minValue) {
            findMinTree(trees.tail, trees.head, trees.head.value)
        } else {
            findMinTree(trees.tail, minTree, minValue)
        }
    }

    // Возвращает список деревьев без дерева с указанным ключом
    private fun deleteTreeByKey(trees: FList<BinomialTree<T>?>, key: T): FList<BinomialTree<T>?> {
        return if ((trees as FList.Cons).head?.value == key) {
            FList.Cons(null, trees.tail)
        } else {
            FList.Cons(trees.head, deleteTreeByKey(trees.tail, key))
        }
    }
}