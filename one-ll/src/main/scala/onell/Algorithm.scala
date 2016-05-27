package onell

import java.util.Random

/**
  * A trait for algorithms solving pseudo-Boolean mutation-aware problems.
  */
trait Algorithm[F] {
  /**
    * Returns the name of the algorithm.
    * @return the name of the algorithm.
    */
  def name: String

  /**
    * Returns the revision string of the algorithm. If one changes this string, all experiments will be re-evaluated.
    * @return the revision string of the algorithm.
    */
  def revision: String

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
  def solve(problem: MutationAwarePseudoBooleanProblem[F])(implicit rng: Random): Seq[Double]
}
