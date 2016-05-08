package onell.util

/**
  * A mutable integer set with fast addition, query, iteration and cleanup operations.
  */
class MutableIntSet(maxElement: Int) extends IntSet {
  private final val contained = Array.ofDim[Boolean](maxElement)
  private final val elements = Array.ofDim[Int](maxElement)
  private var mySize = 0

  /**
    * Clears this set.
    */
  def clear(): Unit = {
    var i = 0
    while (i < mySize) {
      contained(elements(i)) = false
      i += 1
    }
    mySize = 0
  }

  override def size: Int = mySize
  override def apply(element: Int): Boolean = contained(element)
  @inline
  override final def foreach(fun: Int => Unit): Unit = {
    var i = 0
    while (i < mySize) {
      fun(elements(i))
      i += 1
    }
  }
  override def fill(array: Array[Int]): Int = {
    System.arraycopy(elements, 0, array, 0, mySize)
    mySize
  }

  /**
    * Adds the given element to the set.
    *
    * @param element the element to be added.
    */
  def add(element: Int): Unit = {
    if (!contained(element)) {
      contained(element) = true
      elements(mySize) = element
      mySize += 1
    }
  }

  /**
    * Adds the given element to the set.
    *
    * @param element the element to be added.
    */
  final def += (element: Int): Unit = add(element)
}
