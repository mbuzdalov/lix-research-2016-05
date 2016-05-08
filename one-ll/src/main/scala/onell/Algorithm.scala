package onell

import scala.util.Random

/**
  * A trait for algorithms solving pseudo-Boolean mutation-aware problems.
  */
trait Algorithm {
  /**
    * Returns the name of the algorithm.
    * @return the name of the algorithm.
    */
  def name: String

  /**
    * Returns the description of the performance metrics.
    * @return the description of the performance metrics.
    */
  def metrics: Seq[String]

  /**
    * Solves the given problem and returns the performance metrics (for meaning of these metrics,
    * see the return of `metrics`).
    * @param problem the problem to be solved.
    * @param rng the random number generator to use.
    * @return the performance metrics.
    */
  def solve(problem: MutationAwarePseudoBooleanProblem)(implicit rng: Random): Seq[Long]
}
