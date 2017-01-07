package onell

import java.io.{File, PrintWriter}
import java.util.Random
import java.util.concurrent.ThreadLocalRandom

import onell.util.CartesianTree

/**
  * A model of the initial phase in LeadingOnesTrailingZeros.
  */
object LOTZInitModel {
  implicit def rng: Random = ThreadLocalRandom.current()

  private final val orderingFirstIncreasing = Ordering.by((p: (Int, Int)) => p._1)
  private final val orderingSecondDecreasing = Ordering.by((p: (Int, Int)) => -p._2)

  def iteration(
    state: CartesianTree[(Int, Int)],
    split: CartesianTree.SplitResult[(Int, Int)],
    problemSize: Int,
    maxSize: Int,
    iterations: Long,
    iterationLimit: Long
  ): Int = {
    if (iterations < iterationLimit) {
      val index = rng.nextInt(state.size)
      val (x, y) = state.apply(index)
      val newX = math.min(x, rng.nextInt(problemSize))
      val newY = y + 1
      val newK = (newX, newY)
      state.split(newK, split)(orderingFirstIncreasing)
      if (split.m.size > 0 && split.m.last._2 >= newY ||
          split.r.size > 0 && split.r.head._2 >= newY) {
        iteration(state, split, problemSize, maxSize, iterations + 1, iterationLimit)
      } else {
        val rightPart = split.r
        split.l.split(newK, split)(orderingSecondDecreasing)
        val newTree = split.l.merge(CartesianTree(newK)).merge(rightPart)
        iteration(newTree, split, problemSize, math.max(maxSize, newTree.size), iterations + 1, iterationLimit)
      }
    } else {
      maxSize
    }
  }

  def main(args: Array[String]): Unit = {
    val sizes = Seq(100, 1000, 10000, 100000, 1000000, 10000000, 100000000)
    val cacheDir = "cache/lotz-model-1"
    for (n <- sizes; t <- sizes) {
      val runs = 100
      val results = for (r <- (1 to runs).par) yield {
        val file = new File(s"$cacheDir/$n-$t/$r")
        file.getParentFile.mkdirs()
        if (file.exists()) {
          val source = scala.io.Source.fromFile(file)
          val rv = source.getLines().next().toLong
          source.close()
          rv
        } else {
          val timeStart = System.nanoTime()
          val rv = iteration(CartesianTree((n / 2, 0)), new CartesianTree.SplitResult[(Int, Int)], n, 1, 0, t)
          val pw = new PrintWriter(file)
          pw.println(rv)
          pw.println(s"Wall-clock time: ${(System.nanoTime() - timeStart) / 1e9} seconds")
          pw.close()
          rv
        }
      }
      val max = results.max
      val sum = results.sum
      println(s"n = $n, t = $t: max = $max, avg = ${sum.toDouble / runs}")
    }
  }
}
