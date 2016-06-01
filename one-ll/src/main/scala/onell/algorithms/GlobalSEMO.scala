package onell.algorithms

import java.util.Random

import onell.algorithms.GlobalSEMO.Individual
import onell.{Algorithm, Mutation, MutationAwarePseudoBooleanProblem}

/**
  * The Global SEMO algorithm.
  */
abstract class GlobalSEMO extends Algorithm[(Int, Int)] {
  override def name: String = "GlobalSEMO"
  override def metrics: Seq[String] = Seq("Fitness evaluations", "Front hitting time", "Front hitting population size")
  override def revision: String = "rev6"

  override def solve(problem: MutationAwarePseudoBooleanProblem[(Int, Int)])(implicit rng: Random): Seq[Double] = {
    val mutation = new Mutation(problem.problemSize, 1.0 / problem.problemSize)
    def work(population: Array[Individual], iterationsDone: Long, frontHitting: Option[(Long, Int)]): (Long, Long, Int) = {
      if (population.length == problem.numberOfOptimumFitnessValues && population.forall(i => problem.isOptimumFitness(i.fitness))) {
        // Found the entire front
        val (frontHittingTime, hittingPopulationSize) = frontHitting.get
        (iterationsDone, frontHittingTime, hittingPopulationSize)
      } else {
        val index = select(population, rng)
        mutation.createRandomBits(false)
        // First, create the mutant in-place and check if it is dominated by its parent
        val theArray = population(index).bits
        val oldFitness = population(index).fitness
        val newFitness = problem.apply(theArray, oldFitness, mutation)
        if (oldFitness != newFitness && dominates(problem.problemSize, oldFitness, newFitness)) {
          // The new one is worse than its parent, revert and apply failure to the parent
          mutation.mutate(theArray)
          population(index) = population(index).applyFailure
          work(population, iterationsDone + 1, frontHitting)
        } else {
          // The new one is not worse than its parent.
          val newFrontHitting = if (frontHitting.isDefined) frontHitting else {
            if (problem.isOptimumFitness(newFitness)) {
              Some(iterationsDone, population.length)
            } else None
          }
          if (oldFitness == newFitness) {
            // Special case when we are trying to replace the parent.
            // In this case, the mutation is left applied, but no other stuff changes.
            work(population, iterationsDone + 1, frontHitting)
          } else {
            // First check that it is not worse than others
            val notBad = population.forall(i => i.fitness == newFitness || !dominates(problem.problemSize, i.fitness, newFitness))
            if (!notBad) {
              // Found an individual which strictly dominates us, revert and apply failure to the parent
              mutation.mutate(theArray)
              population(index) = population(index).applyFailure
              work(population, iterationsDone + 1, newFrontHitting)
            } else {
              val equalIndex = population.indexWhere(i => !(i.bits eq theArray) && i.fitness == newFitness)
              if (equalIndex >= 0) {
                // If someone's fitness equals the new fitness,
                // silently replace it with the new one and apply NEITHER success NOR failure to the parent.
                // It should be safe to reuse the replaced individual's bit array.
                val tmpArray = population(equalIndex).bits
                System.arraycopy(theArray, 0, tmpArray, 0, theArray.length)
                population(equalIndex) = Individual(tmpArray, newFitness, population(equalIndex).failures)
                mutation.mutate(theArray)
                work(population, iterationsDone + 1, newFrontHitting)
              } else {
                // The general stuff. First, split off the new individual and revert the parent
                val newIndividual = Individual(theArray.clone(), newFitness, 0)
                mutation.mutate(theArray)
                population(index) = population(index).applySuccess
                val builder = Array.newBuilder[Individual]
                var i = 0
                while (i < population.length && population(i).fitness._1 < newFitness._1) {
                  if (!dominates(problem.problemSize, newFitness, population(i).fitness)) {
                    builder += population(i)
                  }
                  i += 1
                }
                builder += newIndividual
                while (i < population.length) {
                  if (!dominates(problem.problemSize, newFitness, population(i).fitness)) {
                    builder += population(i)
                  }
                  i += 1
                }
                val newPopulation = builder.result()
                work(newPopulation, iterationsDone + 1, newFrontHitting)
              }
            }
          }
        }
      }
    }
    val initIndividual = Array.fill(problem.problemSize)(rng.nextBoolean())
    val initFitness = problem.apply(initIndividual)
    val (iterations, frontHittingTime, hittingPopulationSize) = work(
      Array(Individual(initIndividual, initFitness, 0)), 1,
      if (problem.isOptimumFitness(initFitness)) Some((1L, 1)) else None
    )
    Seq(iterations.toDouble, frontHittingTime.toDouble, hittingPopulationSize.toDouble)
  }

  private def dominates(size: Int, master: (Int, Int), slave: (Int, Int)): Boolean = {
    master._1 >= slave._1 && master._2 >= slave._2 && dominationRefinement(size, master, slave)
  }

  def select(population: Array[Individual], rng: Random): Int
  def dominationRefinement(problemSize: Int, master: (Int, Int), slave: (Int, Int)): Boolean
}

object GlobalSEMO {
  case class Individual(bits: Array[Boolean], fitness: (Int, Int), failures: Int) {
    def applySuccess: Individual = if (failures == 0) this else copy(failures = 0)
    def applyFailure: Individual = copy(failures = this.failures + 1)
  }

  object Selection {
    trait Uniform extends GlobalSEMO {
      override def name: String = super.name + "[selection=uniform]"
      override def select(population: Array[Individual], rng: Random) = rng.nextInt(population.length)
    }

    trait Fertility extends GlobalSEMO {
      override def name: String = super.name + "[selection=fertility]"
      override def select(population: Array[Individual], rng: Random) = {
        var rv = 0
        var builder = Array.newBuilder[Int]
        for (i <- 0 until population.length) {
          if (population(i).failures < population(rv).failures) {
            rv = i
            builder.clear()
          }
          if (population(i).failures == population(rv).failures) {
            builder += i
          }
        }
        val allIndices = builder.result()
        allIndices(rng.nextInt(allIndices.length))
      }
    }

    trait Crowding extends GlobalSEMO {
      private final val extremeProbability = 1.0 / 3
      private final val maxCrowdingProbability = 1.0 / 3
      override def name: String = super.name + "[selection=crowding]"
      override def select(population: Array[Individual], rng: Random) = {
        val selectionProbability = rng.nextDouble()
        if (selectionProbability < extremeProbability) {
          if (rng.nextBoolean()) 0 else population.length - 1
        } else if ((selectionProbability - extremeProbability) < maxCrowdingProbability) {
          val best = Array.newBuilder[Int]
          var bestCrowding = 0.0
          val globalDX: Double = population.head.fitness._1 - population.last.fitness._1
          val globalDY: Double = population.last.fitness._2 - population.head.fitness._2
          for (i <- 1 until population.length - 1) {
            val diffX = population(i - 1).fitness._1 - population(i + 1).fitness._1
            val diffY = population(i + 1).fitness._2 - population(i - 1).fitness._2
            val crowding = diffX / globalDX + diffY / globalDY
            if (crowding > bestCrowding) {
              bestCrowding = crowding
              best.clear()
            }
            if (crowding == bestCrowding) {
              best += i
            }
          }
          if (bestCrowding == 0) {
            if (rng.nextBoolean()) 0 else population.length - 1
          } else {
            val array = best.result()
            array(rng.nextInt(array.length))
          }
        } else {
          rng.nextInt(population.length)
        }
      }
    }
  }

  object Niching {
    trait None extends GlobalSEMO {
      override def name: String = super.name + "[niching=none]"
      override def dominationRefinement(problemSize: Int, master: (Int, Int), slave: (Int, Int)): Boolean = true
    }

    trait Parallel extends GlobalSEMO {
      override def name: String = super.name + "[niching=parallel]"
      override def dominationRefinement(problemSize: Int, master: (Int, Int), slave: (Int, Int)): Boolean = {
        val niche = math.sqrt(problemSize).toInt
        val masterNiche = (master._1 - master._2 + problemSize) / niche
        val slaveNiche = (slave._1 - slave._2 + problemSize) / niche
        masterNiche == slaveNiche
      }
    }

    trait Distance extends GlobalSEMO {
      override def name: String = super.name + "[niching=distance]"
      override def dominationRefinement(problemSize: Int, master: (Int, Int), slave: (Int, Int)): Boolean = {
        val niche = math.sqrt(problemSize).toInt
        val masterE = master._1 - master._2
        val slaveE = slave._1 - slave._2
        math.abs(masterE - slaveE) <= niche
      }
    }
  }
}