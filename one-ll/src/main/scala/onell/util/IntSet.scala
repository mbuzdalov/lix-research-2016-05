package onell.util

/**
  * An abstract integer set interface.
  */
trait IntSet {

  /**
    * Returns the size of the set (i.e. the number of elements contained in the set).
    *
    * @return the size of the set.
    */
  def size: Int

  /**
    * Tests if the given element is contained in the set.
    *
    * @param element the element to test.
    * @return true if the element is contained in the set, false otherwise.
    */
  def apply(element: Int): Boolean

  /**
    * Invokes the given function on every element from the set.
    *
    * @param fun the function to invoke.
    */
  @inline
  def foreach(fun: Int => Unit): Unit

  /**
    * Fills the given array with the elements contained in the set, starting from the zeroth index.
    * Returns the number of elements.
    *
    * @param array the array to be filled.
    * @return the number of elements put into the array.
    */
  def fill(array: Array[Int]): Int
}
