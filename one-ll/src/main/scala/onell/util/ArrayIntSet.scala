package onell.util

/**
  * A mutable integer set with fast addition, query, iteration and cleanup operations.
  */
class ArrayIntSet(maxElement: Int) extends IntSet {
  protected final val contained: Array[Boolean] = Array.ofDim(maxElement)
  protected final val elements: Array[Int] = Array.ofDim(maxElement)
  private[this] var mySize = 0

  /**
    * Clears this set.
    */
  protected def clear(): Unit = {
    var i = 0
    val iMax = mySize
    val cntd = contained
    val elms = elements
    while (i < iMax) {
      cntd(elms(i)) = false
      i += 1
    }
    mySize = 0
  }

  override final def size: Int = mySize
  override final def apply(element: Int): Boolean = contained(element)
  @inline
  override final def foreach(fun: Int => Unit): Unit = {
    var i = 0
    val iMax = mySize
    val elms = elements
    while (i < iMax) {
      fun(elms(i))
      i += 1
    }
  }

  @inline
  override final def count(predicate: Int => Boolean): Int = {
    var i = 0
    var cnt = 0
    val iMax = mySize
    val elms = elements
    while (i < iMax) {
      if (predicate(elms(i))) {
        cnt += 1
      }
      i += 1
    }
    cnt
  }

  override final def fill(array: Array[Int]): Int = {
    System.arraycopy(elements, 0, array, 0, mySize)
    mySize
  }

  /**
    * Adds the given element to the set.
    *
    * @param element the element to be added.
    */
  protected def += (element: Int): Unit = {
    if (!contained(element)) {
      contained(element) = true
      elements(mySize) = element
      mySize += 1
    }
  }
}
