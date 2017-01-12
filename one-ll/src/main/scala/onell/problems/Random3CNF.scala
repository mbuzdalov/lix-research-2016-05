package onell.problems

import java.util.concurrent.ThreadLocalRandom

import onell.MutationAwarePseudoBooleanProblem.Instance
import onell.util.MutableIntSet
import onell.{Mutation, MutationAwarePseudoBooleanProblem}

/**
  * A random planted-solution 3-CNF-SAT instance.
  */
class Random3CNF(n: Int, m: Int) extends MutationAwarePseudoBooleanProblem[Int] {
  override def name: String = s"Random3CNF($n,$m)"
  override def newInstance: Instance[Int] = new Random3CNF.Instance(n, m)
}

object Random3CNF {
  final class Instance(n: Int, m: Int) extends MutationAwarePseudoBooleanProblem.Instance[Int] {
    private val rng = ThreadLocalRandom.current()
    private val assignment = Array.fill(n)(rng.nextBoolean())
    private val clauseVar = Array.ofDim[Int](3 * m)
    private val clauseVal = Array.ofDim[Boolean](3 * m)

    private def isOk(clauseIndex: Int, solution: Array[Boolean]): Boolean = {
      val i1 = 3 * clauseIndex
      val i2 = i1 + 1
      val i3 = i2 + 1
      solution(clauseVar(i1)) == clauseVal(i1) ||
        solution(clauseVar(i2)) == clauseVal(i2) ||
        solution(clauseVar(i3)) == clauseVal(i3)
    }

    private val clausesOfVar = {
      val builders = Array.fill(n)(Array.newBuilder[Int])
      for (i <- 0 until m) {
        val i1 = 3 * i
        val i2 = i1 + 1
        val i3 = i2 + 1
        do {
          clauseVar(i1) = rng.nextInt(n)
          clauseVar(i2) = rng.nextInt(n)
          clauseVar(i3) = rng.nextInt(n)
          clauseVal(i1) = rng.nextBoolean()
          clauseVal(i2) = rng.nextBoolean()
          clauseVal(i3) = rng.nextBoolean()
        } while (!isOk(i, assignment))
        builders(clauseVar(i1)) += i
        builders(clauseVar(i2)) += i
        builders(clauseVar(i3)) += i
      }
      builders.map(_.result())
    }

    private var usedClauses = new MutableIntSet(m)

    override def equivalenceFollows(fitness: Int): Boolean = false
    override def isOptimumFitness(fitness: Int): Boolean = fitness == m
    override def numberOfOptimumFitnessValues: Int = 1
    override def problemSize: Int = n

    override def apply(solution: Array[Boolean]): Int = (0 until m).count(i => isOk(i, solution))
    override def apply(solution: Array[Boolean],
                       originalFitness: Int,
                       mutation: Mutation
                      ): Int = {
      usedClauses.clear()
      for (i <- mutation) {
        val localClauses = clausesOfVar(i)
        var j = localClauses.length - 1
        while (j >= 0) {
          usedClauses += localClauses(j)
          j -= 1
        }
      }
      var newFitness = originalFitness
      for (i <- usedClauses) {
        if (isOk(i, solution)) {
          newFitness -= 1
        }
      }
      mutation.mutate(solution)
      for (i <- usedClauses) {
        if (isOk(i, solution)) {
          newFitness += 1
        }
      }
      newFitness
    }
  }
}
