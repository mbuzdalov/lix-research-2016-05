package onell.algorithms

import java.util.Random
import java.util.concurrent.ThreadLocalRandom

import onell.{Algorithm, Mutation, MutationAwarePseudoBooleanProblem}

/**
  * The (1+1)-EA algorithm.
  */
object OnePlusOneEA extends Algorithm[Int] {
  override def name: String = "(1+1)-EA"
  override def revision: String = "rev0 +randomfix"
  override def metrics: Seq[String] = Seq("Fitness evaluations")
  override def solve(problem: MutationAwarePseudoBooleanProblem[Int]): Seq[Double] = {
    val n = problem.problemSize
    val rng = ThreadLocalRandom.current()
    val mutation = new Mutation(n, 1.0 / n, rng)

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
