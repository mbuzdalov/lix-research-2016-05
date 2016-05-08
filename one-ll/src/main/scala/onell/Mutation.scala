package onell

import onell.util.{IntSet, MutableIntSet}

import scala.util.Random

/**
  * A mutation operator which generates mutation indices
  * based on the problem size `n` and the mutation probability `p`.
  */
class Mutation(n: Int, initialP: Double)(implicit rng: Random) extends IntSet {
  private var p = initialP
  private var log1p = math.log(1 - p)
  private final val indices = new MutableIntSet(n)

  private def offset() = if (p == 1) 1 else 1 + (math.log(rng.nextDouble()) / log1p).toInt

  @inline
  override final def foreach(fun: (Int) => Unit): Unit = indices.foreach(fun)
  override def size: Int                               = indices.size
  override def apply(element: Int): Boolean            = indices(element)
  override def fill(array: Array[Int]): Int            = indices.fill(array)

  def setProbability(newP: Double): Unit = {
    p = newP
    log1p = math.log(1 - p)
  }

  /**
    * Creates a mutation pattern by choosing random bits (with probability `p` given at the construction).
    * If `useSameIndexCount` is true, it chooses exactly this many bits as was chosen last time.
    * @param useSameIndexCount whether to choose the same number of bits as last time.
    */
  def createRandomBits(useSameIndexCount: Boolean): Unit = {
    if (!useSameIndexCount) {
      indices.clear()
      var index = offset() - 1
      while (index < n) {
        indices.add(index)
        index += offset()
      }
    } else {
      val count = indices.size
      indices.clear()
      while (indices.size < count) {
        indices.add(rng.nextInt(n))
      }
    }
  }

  /**
    * Creates a mutation pattern by choosing random bits among the ones presented in the given array.
    * @param bitArray the array with bits.
    * @param bitCount the number of bits in the array to take.
    */
  def chooseRandomBits(bitArray: Array[Int], bitCount: Int): Unit = {
    //Seems to be the same but I am unsure
    indices.clear()
    var index = offset() - 1
    while (index < bitCount) {
      indices.add(bitArray(index))
      index += offset()
    }
  }

  /**
    * Applies the mutation to the given bits.
    * @param bits the bits to mutate.
    */
  def mutate(bits: Array[Boolean]): Unit = {
    for (i <- indices) {
      bits(i) ^= true
    }
  }

  /**
    * Undoes the mutation to the given bits.
    * @param bits the bits to undo the mutation on.
    */
  def undo(bits: Array[Boolean]): Unit = mutate(bits)
}
