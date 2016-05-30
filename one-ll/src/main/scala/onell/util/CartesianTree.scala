package onell.util

import java.util.Random

import onell.util.CartesianTree.SplitResult

import scala.collection.generic.Growable

/**
  * A Cartesian tree for bi-objective non-dominated set maintenance and reproduction selection.
  */
trait CartesianTree[+D] {
  def left: CartesianTree[D]
  def right: CartesianTree[D]
  def data: D
  def subtreeSize: Int

  def head: D = chooseAt(0)
  def last: D = chooseAt(subtreeSize - 1)

  def chooseAt(index: Int): D

  def split[D1 >: D](key: D1, result: SplitResult[D1])(implicit ordering: Ordering[D1]): Unit
  def merge[D1 >: D](right: CartesianTree[D1]): CartesianTree[D1]

  protected def flushToBuilder(builder: Growable[D]): Unit

  def toIndexedSeq: IndexedSeq[D] = {
    val builder = IndexedSeq.newBuilder[D]
    flushToBuilder(builder)
    builder.result()
  }
}

object CartesianTree {
  val EmptyTree: CartesianTree[Nothing] = new CartesianTree[Nothing] {
    override final val subtreeSize: Int = 0
    override final val left: CartesianTree[Nothing] = this
    override final val right: CartesianTree[Nothing] = this
    override def chooseAt(index: Int): Nothing = throw new UnsupportedOperationException("Cannot choose anything in an empty tree")
    override def data: Nothing = throw new UnsupportedOperationException("No data in an empty tree")
    override def merge[D1 >: Nothing](right: CartesianTree[D1]) = right
    override def flushToBuilder(builder: Growable[Nothing]): Unit = {}
    override def split[D1 >: Nothing](key: D1, result: SplitResult[D1])(implicit ordering: Ordering[D1]): Unit = {
      result.l = this
      result.m = this
      result.r = this
    }
  }

  def apply[D](data: D)(implicit rng: Random): CartesianTree[D] = {
    NonemptyTree(EmptyTree, EmptyTree, data, rng.nextInt())
  }

  private case class NonemptyTree[+D](
    left: CartesianTree[D],
    right: CartesianTree[D],
    data: D,
    heapKey: Int
  ) extends CartesianTree[D] {
    override final val subtreeSize = 1 + left.subtreeSize + right.subtreeSize

    override def chooseAt(index: Int): D = {
      if (index < left.subtreeSize) {
        left.chooseAt(index)
      } else if (index == left.subtreeSize) {
        data
      } else {
        right.chooseAt(index - left.subtreeSize - 1)
      }
    }

    override def split[D1 >: D](key: D1, result: SplitResult[D1])(implicit ordering: Ordering[D1]): Unit = {
      val compare = ordering.compare(key, data)
      if (compare == 0) {
        result.l = left
        result.r = right
        result.m = this.copy(left = EmptyTree, right = EmptyTree)
      } else if (compare < 0) {
        left.split(key, result)
        result.r = this.copy(left = result.r)
      } else {
        right.split(key, result)
        result.l = this.copy(right = result.l)
      }
    }

    override def merge[D1 >: D](right: CartesianTree[D1]): CartesianTree[D1] = {
      right match {
        case EmptyTree => this
        case that: NonemptyTree[D1] =>
          if (heapKey < that.heapKey) {
            this.copy(right = this.right.merge(that))
          } else {
            that.copy(left = this.merge(that.left))
          }
      }
    }

    override def flushToBuilder(builder: Growable[D]): Unit = {
      left.flushToBuilder(builder)
      builder += data
      right.flushToBuilder(builder)
    }
  }

  class SplitResult[D] {
    var l, m, r: CartesianTree[D] = null
  }
}
