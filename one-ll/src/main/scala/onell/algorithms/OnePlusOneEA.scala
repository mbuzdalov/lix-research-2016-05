package onell.algorithms

import java.util.Random

import onell.{Algorithm, Mutation, MutationAwarePseudoBooleanProblem}

/**
  * The (1+1)-EA algorithm.
  */
object OnePlusOneEA extends Algorithm[Int] {
  override def name: String = "(1+1)-EA"
  override def revision: String = "rev0 +randomfix"
  override def metrics: Seq[String] = Seq("Fitness evaluations")
  override def solve(problem: MutationAwarePseudoBooleanProblem[Int])(implicit rng: Random): Seq[Double] = {
    val n = problem.problemSize
    val mutation = new Mutation(n, 1.0 / n)

    val individual = Array.fill(n)(rng.nextBoolean())
    var evaluations = 1L
    var fitness = problem(individual)

    while (!problem.isOptimumFitness(fitness)) {
      mutation.createRandomBits(false)
      val newFitness = problem(individual, fitness, mutation)
      evaluations += 1
      if (newFitness >= fitness) {
        fitness = newFitness
      } else {
        mutation.undo(individual)
      }
    }

    Seq(evaluations)
  }
}
