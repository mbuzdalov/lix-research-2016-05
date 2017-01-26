package onell.problems

import java.util.concurrent.ThreadLocalRandom

import onell.util.MutableIntSet
import onell.{Mutation, MutationAwarePseudoBooleanProblem}

import scala.annotation.tailrec

/**
  * A random planted-solution 3-CNF-SAT instance.
  */
class Random3CNF(n: Int, m: Int) extends MutationAwarePseudoBooleanProblem[Int] {
  override def name: String = s"Random3CNF($n,$m)"
  override def newInstance = new Random3CNF.Instance(n, m)
}

object Random3CNF {
  final class Instance(n: Int, m: Int) extends MutationAwarePseudoBooleanProblem.Instance[Int] {
    private val assignment = Array.ofDim[Boolean](n)
    private val clauseVar = Array.ofDim[Int](3 * m)
    private val clauseVal = Array.ofDim[Boolean](3 * m)
    private val clausesOfVarValues = Array.ofDim[Int](3 * m)
    private val clausesOfVarIndices = Array.ofDim[Int](n + 1)
    private val usedClauses = new MutableIntSet(m)

    private def isOk(clauseIndex: Int, clauseVal: Array[Boolean], clauseVar: Array[Int], solution: Array[Boolean]): Boolean = {
      val i1 = 3 * clauseIndex
      val i2 = i1 + 1
      val i3 = i2 + 1
      solution(clauseVar(i1)) == clauseVal(i1) || solution(clauseVar(i2)) == clauseVal(i2) || solution(clauseVar(i3)) == clauseVal(i3)
    }

    @tailrec
    private def initAssignment(i: Int, n: Int, assignment: Array[Boolean], rng: ThreadLocalRandom): Unit = if (i < n) {
      assignment(i) = rng.nextBoolean()
      initAssignment(i + 1, n, assignment, rng)
    }
    @tailrec
    private def initClause(
      i: Int, i1: Int, i2: Int, i3: Int, n: Int, assignment: Array[Boolean],
      clauseVal: Array[Boolean], clauseVar: Array[Int], degrees: Array[Int], rng: ThreadLocalRandom
    ): Unit = {
      clauseVar(i1) = rng.nextInt(n)
      clauseVar(i2) = rng.nextInt(n)
      clauseVar(i3) = rng.nextInt(n)
      clauseVal(i1) = rng.nextBoolean()
      clauseVal(i2) = rng.nextBoolean()
      clauseVal(i3) = rng.nextBoolean()
      if (!isOk(i, clauseVal, clauseVar, assignment)) {
        initClause(i, i1, i2, i3, n, assignment, clauseVal, clauseVar, degrees, rng)
      } else {
        degrees(clauseVar(i1)) += 1
        degrees(clauseVar(i2)) += 1
        degrees(clauseVar(i3)) += 1
      }
    }
    @tailrec
    private def initClauses(
      i: Int, n: Int, m: Int, assignment: Array[Boolean], clauseVal: Array[Boolean],
      clauseVar: Array[Int], degrees: Array[Int], rng: ThreadLocalRandom
    ): Unit = {
      if (i < m) {
        initClause(i, 3 * i, 3 * i + 1, 3 * i + 2, n, assignment, clauseVal, clauseVar, degrees, rng)
        initClauses(i + 1, n, m, assignment, clauseVal, clauseVar, degrees, rng)
      }
    }
    @tailrec
    private def makePartialSums(i: Int, n: Int, degrees: Array[Int]): Unit = if (i <= n) {
      degrees(i) += degrees(i - 1)
      makePartialSums(i + 1, n, degrees)
    }
    @tailrec
    private def fillResultArrays(
      i: Int, m: Int, degrees: Array[Int], values: Array[Int], clauseVar: Array[Int]
    ): Unit = if (i < m) {
      val j1 = clauseVar(3 * i)
      val j2 = clauseVar(3 * i + 1)
      val j3 = clauseVar(3 * i + 2)
      degrees(j1) -= 1
      values(degrees(j1)) = i
      degrees(j2) -= 1
      values(degrees(j2)) = i
      degrees(j3) -= 1
      values(degrees(j3)) = i
      fillResultArrays(i + 1, m, degrees, values, clauseVar)
    }

    initAssignment(0, n, assignment, ThreadLocalRandom.current())
    initClauses(0, n, m, assignment, clauseVal, clauseVar, clausesOfVarIndices, ThreadLocalRandom.current())
    makePartialSums(1, n, clausesOfVarIndices)
    fillResultArrays(0, m, clausesOfVarIndices, clausesOfVarValues, clauseVar)

    override def equivalenceFollows(fitness: Int): Boolean = false
    override def isOptimumFitness(fitness: Int): Boolean = fitness == m
    override def numberOfOptimumFitnessValues: Int = 1
    override def problemSize: Int = n

    def distance(solution: Array[Boolean]): Int = (0 until n).count(i => solution(i) != assignment(i))

    override def apply(solution: Array[Boolean]): Int = {
      val cvl = clauseVal
      val cvr = clauseVar
      (0 until m).count(i => isOk(i, cvl, cvr, solution))
    }
    override def apply(solution: Array[Boolean],
                       originalFitness: Int,
                       mutation: Mutation
                      ): Int = {
      if (3 * mutation.size < n) {
        val uc = usedClauses
        val cvi = clausesOfVarIndices
        val cvv = clausesOfVarValues
        val cvl = clauseVal
        val cvr = clauseVar
        uc.clear()
        for (i <- mutation) {
          var j = cvi(i)
          val jMax = cvi(i + 1)
          while (j < jMax) {
            uc += cvv(j)
            j += 1
          }
        }
        val fitnessDecrease = uc.count(i => isOk(i, cvl, cvr, solution))
        mutation.mutate(solution)
        val fitnessIncrease = uc.count(i => isOk(i, cvl, cvr, solution))
        originalFitness + fitnessIncrease - fitnessDecrease
      } else {
        // This path should be faster for very large mutations
        mutation.mutate(solution)
        apply(solution)
      }
    }
  }
}
