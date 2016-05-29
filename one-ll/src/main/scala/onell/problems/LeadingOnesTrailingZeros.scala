package onell.problems

import onell.{Mutation, MutationAwarePseudoBooleanProblem}

/**
  * The (LeadingOnes, TrailingZeros) bi-objective problem implemented as a mutation-aware pseudo-Boolean problem.
  */
class LeadingOnesTrailingZeros(n: Int) extends MutationAwarePseudoBooleanProblem[(Int, Int)] {
  override def copy = this
  override def name: String = s"LeadingOnesTrailingZeros($n)"
  override def isOptimumFitness(fitness: (Int, Int)): Boolean = fitness._1 + fitness._2 == n
  override def numberOfOptimumFitnessValues = n + 1
  override def problemSize: Int = n
  override def apply(solution: Array[Boolean]): (Int, Int) = {
    val firstFalse = solution.indexOf(false)
    (if (firstFalse == -1) n else firstFalse, n - 1 - solution.lastIndexOf(true))
  }
  override def apply(solution: Array[Boolean], originalFitness: (Int, Int), mutation: Mutation): (Int, Int) = {
    var firstFlippedIndex = n
    var lastFlippedIndex = -1
    for (i <- mutation) {
      if (i < firstFlippedIndex) {
        firstFlippedIndex = i
      }
      if (i > lastFlippedIndex) {
        lastFlippedIndex = i
      }
      solution(i) ^= true
    }
    val newLeadingOnes = if (firstFlippedIndex < originalFitness._1) {
      firstFlippedIndex
    } else if (firstFlippedIndex == originalFitness._1) {
      var lo = firstFlippedIndex
      while (lo < n && solution(lo)) {
        lo += 1
      }
      lo
    } else originalFitness._1
    val newTrailingZeros = if (n - 1 - lastFlippedIndex < originalFitness._2) {
      n - 1 - lastFlippedIndex
    } else if (n - 1 - lastFlippedIndex == originalFitness._2) {
      var invTZ = lastFlippedIndex
      while (invTZ >= 0 && !solution(invTZ)) {
        invTZ -= 1
      }
      n - 1 - invTZ
    } else originalFitness._2
    (newLeadingOnes, newTrailingZeros)
  }
}
