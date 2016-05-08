package onell.algorithms

import onell.{Algorithm, Mutation, MutationAwarePseudoBooleanProblem}

import scala.util.Random

/**
  * The (1+1)-EA algorithm.
  */
object OnePlusOneEA extends Algorithm {
  override def name: String = "(1+1)-EA"
  override def metrics: Seq[String] = Seq("Fitness evaluations")
  override def solve(problem: MutationAwarePseudoBooleanProblem)(implicit rng: Random): Seq[Long] = {
    val n = problem.problemSize
    val m = problem.optimumFitness
    val mutation = new Mutation(n, 1.0 / n)

    val individual = Array.fill(n)(rng.nextBoolean())
    var evaluations = 1L
    var fitness = problem(individual)

    while (fitness < m) {
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
