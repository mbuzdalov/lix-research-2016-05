package onell.problems

import onell.{Mutation, MutationAwarePseudoBooleanProblem}

/**
  * The OneZeroMax bi-objective problem implemented as a mutation-aware pseudo-Boolean problem.
  * OneMax is the first objective, ZeroMax is the second one.
  */
class OneZeroMax(n: Int) extends MutationAwarePseudoBooleanProblem[(Int, Int)] {
  override def copy = this
  override def name: String = s"OneZeroMax($n)"
  override def isOptimumFitness(fitness: (Int, Int)): Boolean = true
  override def numberOfOptimumFitnessValues = n + 1
  override def problemSize: Int = n
  override def apply(solution: Array[Boolean]): (Int, Int) = {
    val oneMax = (0 until n).count(solution)
    (oneMax, n - oneMax)
  }
  override def apply(solution: Array[Boolean], originalFitness: (Int, Int), mutation: Mutation): (Int, Int) = {
    var newOneMax = originalFitness._1
    for (i <- mutation) {
      if (solution(i)) {
        newOneMax -= 1
      } else {
        newOneMax += 1
      }
    }
    mutation.mutate(solution)
    (newOneMax, n - newOneMax)
  }
}
