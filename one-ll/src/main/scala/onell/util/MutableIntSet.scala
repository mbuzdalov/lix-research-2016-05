package onell.util

/**
  * A mutable integer set with fast addition, query, iteration and cleanup operations.
  */
class MutableIntSet(maxElement: Int) extends ArrayIntSet(maxElement) {
  override final def clear(): Unit = super.clear()
  override final def += (element: Int): Unit = super.+=(element)
}
