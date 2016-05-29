package onell.algorithms

import java.util.Random

import onell.algorithms.GlobalSEMO.Individual
import onell.{Algorithm, Mutation, MutationAwarePseudoBooleanProblem}

/**
  * The Global SEMO algorithm.
  */
abstract class GlobalSEMO extends Algorithm[(Int, Int)] {
  override def name: String = "GlobalSEMO"
  override def metrics: Seq[String] = Seq("Fitness evaluations")
  override def revision: String = "rev0"

  override def solve(problem: MutationAwarePseudoBooleanProblem[(Int, Int)])(implicit rng: Random): Seq[Double] = {
    val mutation = new Mutation(problem.problemSize, 1.0 / problem.problemSize)
    def work(population: Array[Individual], iterationsDone: Long): Long = {
//      // TODO: Debug: check things are increasing and non-dominating. Remove it as it is quite slow
//      for (i <- 1 until population.length) {
//        assert(population(i - 1).fitness._1 < population(i).fitness._1)
//        assert(population(i - 1).fitness._2 > population(i).fitness._2)
//      }
      if (population.length == problem.numberOfOptimumFitnessValues && population.forall(i => problem.isOptimumFitness(i.fitness))) {
        // Found the entire front
        iterationsDone
      } else {
        val index = select(population, rng)
        mutation.createRandomBits(false)
        // First, create the mutant in-place and check if it is dominated by its parent
        val theArray = population(index).bits
        val oldFitness = population(index).fitness
        val newFitness = problem.apply(theArray, oldFitness, mutation)
        if (dominates(problem.problemSize, oldFitness, newFitness)) {
          // The new one is worse than its parent, revert and apply failure to the parent
          mutation.mutate(theArray)
          population(index) = population(index).applyFailure
          work(population, iterationsDone + 1)
        } else {
          // The new one is not worse than its parent.
          // First check that it is not worse than others
          val notBad = population.forall(i => i.fitness != newFitness && !dominates(problem.problemSize, i.fitness, newFitness))
          if (!notBad) {
            // Found an individual which strictly dominates us, revert and apply failure to the parent
            mutation.mutate(theArray)
            population(index) = population(index).applyFailure
            work(population, iterationsDone + 1)
          } else {
            val equalIndex = population.indexWhere(i => !(i.bits eq theArray) && i.fitness == newFitness)
            if (equalIndex >= 0) {
              // If someone's fitness equals the new fitness,
              // silently replace it with the new one and apply success to the parent.
              // It should be safe to reuse the replaced individual's bit array.
              val tmpArray = population(equalIndex).bits
              System.arraycopy(theArray, 0, tmpArray, 0, theArray.length)
              population(equalIndex) = Individual(tmpArray, newFitness, 0)
              mutation.mutate(theArray)
              population(index) = population(index).applySuccess
              work(population, iterationsDone + 1)
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
              work(builder.result(), iterationsDone + 1)
            }
          }
        }
      }
    }
    val initIndividual = Array.fill(problem.problemSize)(rng.nextBoolean())
    val iterations = work(Array(Individual(initIndividual, problem.apply(initIndividual), 0)), 1)
    Seq(iterations.toDouble)
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
        for (i <- 1 until population.length) {
          if (population(i).failures < population(rv).failures) {
            rv = i
          }
        }
        rv
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
          var best = 0
          var bestCrowding = 0.0
          val globalDX: Double = population.head.fitness._1 - population.last.fitness._1
          val globalDY: Double = population.last.fitness._2 - population.head.fitness._2
          for (i <- 1 until population.length - 1) {
            val diffX = population(i - 1).fitness._1 - population(i + 1).fitness._1
            val diffY = population(i + 1).fitness._2 - population(i - 1).fitness._2
            val crowding = diffX / globalDX + diffY / globalDY
            if (crowding > bestCrowding) {
              bestCrowding = crowding
              best = i
            }
          }
          if (best == 0) {
            if (rng.nextBoolean()) 0 else population.length - 1
          } else best
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
