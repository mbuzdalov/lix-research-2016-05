package onell.algorithms

import java.io.{File, PrintWriter}

import onell.{Algorithm, Mutation, MutationAwarePseudoBooleanProblem}

import scala.util.Random

/**
  * The (1+(L,L))-GA by Doerr, Doerr, Ebel.
  */
class OnePlusLambdaLambdaGA(
  minimalLambda: Double = 1,
  minimalLambdaText: String = "1",
  maximalLambda: Double = Double.PositiveInfinity,
  maximalLambdaText: String = "n"
) extends Algorithm {
  private final val tuningStrength = 1.5
  private final val tuningStrength4 = math.pow(tuningStrength, 0.25)

  override def revision: String = "dump lambdas v1"

  override def name: String = s"(1+LL)[$minimalLambdaText;$maximalLambdaText]"
  override def metrics: Seq[String] = Seq("Fitness evaluations", "Iterations", "Maximal lambda", "Divergence of (lambda / sqrt(n/d))")
  override def solve(problem: MutationAwarePseudoBooleanProblem)(implicit rng: Random): Seq[Double] = {
    val n = problem.problemSize
    val m = problem.optimumFitness
    val mutation = new Mutation(n, minimalLambda / n)
    val crossover = new Mutation(n, 1 / minimalLambda)

    val individual = Array.fill(n)(rng.nextBoolean())
    val optimum = problem.optimalSolution
    var fitness = problem(individual)
    var structuralDistance = (individual, optimum).zipped.count(p => p._1 != p._2)
    var iterations = 1L
    var evaluations = 1L
    var lambda = minimalLambda
    var maxSeenLambda = lambda
    val firstChildDiff = Array.ofDim[Int](n)
    var firstChildDiffCount = 0
    val secondChildDiff = Array.ofDim[Int](n)
    var secondChildDiffCount = 0

    var sumLambdaRatios = 0.0

    val file = new File(s"/tmp/one-ll/${problem.name}/$minimalLambdaText-$maximalLambdaText/${math.abs(rng.nextLong())}")
    file.getParentFile.mkdirs()
    val debugOutput = new PrintWriter(file)

    while (fitness < m) {
      val bestPossibleLambda = math.min(math.sqrt(n.toDouble / structuralDistance), maximalLambda)
      debugOutput.println(s"${iterations + 1}: $structuralDistance, lambda = $lambda / best possible is $bestPossibleLambda / n/d = ${n.toDouble / structuralDistance}")
      mutation.setProbability(lambda / n)
      crossover.setProbability(1 / lambda)
      val ratio = lambda / bestPossibleLambda
      sumLambdaRatios += math.max(ratio, 1.0 / ratio)

      val lambdaInt = lambda.toInt
      var bestFirstChildFitness = -1
      var t = 0
      while (t < lambdaInt) {
        mutation.createRandomBits(t != 0)
        val firstChildFitness = problem(individual, fitness, mutation)
        if (firstChildFitness > bestFirstChildFitness) {
          firstChildDiffCount = mutation.fill(firstChildDiff)
          bestFirstChildFitness = firstChildFitness
        }
        mutation.undo(individual)
        t += 1
      }
      var bestSecondChildFitness = -1
      t = 0
      while (t < lambdaInt) {
        crossover.chooseRandomBits(firstChildDiff, firstChildDiffCount)
        val secondChildFitness = problem(individual, fitness, crossover)
        if (secondChildFitness > bestSecondChildFitness) {
          secondChildDiffCount = crossover.fill(secondChildDiff)
          bestSecondChildFitness = secondChildFitness
        }
        crossover.undo(individual)
        t += 1
      }
      lambda = if (bestSecondChildFitness > fitness) {
        math.max(minimalLambda, lambda / tuningStrength)
      } else {
        math.min(math.min(n, maximalLambda), lambda * tuningStrength4)
      }
      maxSeenLambda = math.max(maxSeenLambda, lambda)
      if (bestSecondChildFitness >= fitness) {
        fitness = bestSecondChildFitness
        var i = 0
        while (i < secondChildDiffCount) {
          if (individual(secondChildDiff(i)) == optimum(secondChildDiff(i))) {
            structuralDistance += 1
          } else {
            structuralDistance -= 1
          }
          individual(secondChildDiff(i)) ^= true
          i += 1
        }
      }
      evaluations += 2 * lambda.toInt
      iterations += 1
    }

    debugOutput.close()

    Seq(evaluations, iterations, maxSeenLambda, sumLambdaRatios / iterations)
  }
}
