package onell

import java.util.Locale

import onell.algorithms.GlobalSEMO
import onell.problems.{LeadingOnesTrailingZeros, OneZeroMax}
import onell.util.Plotter
import onell.util.RunHelpers._
import onell.util.Misc._

import scala.language.implicitConversions

/**
  * The main class which runs experiments for SEMO.
  */
object MainSEMO {
  def getSimpleSEMO    = new GlobalSEMO with GlobalSEMO.Niching.None with GlobalSEMO.Selection.Uniform
  def getCrowdingSEMO  = new GlobalSEMO with GlobalSEMO.Niching.None with GlobalSEMO.Selection.Crowding
  def getFertilitySEMO = new GlobalSEMO with GlobalSEMO.Niching.None with GlobalSEMO.Selection.Fertility

  def main(args: Array[String]): Unit = {
    Locale.setDefault(Locale.US)

    val nThreads = args.getOption("--threads=").map(_.toInt).getOrElse(Runtime.getRuntime.availableProcessors())
    val nRuns = args.getOption("--runs=").map(_.toInt).getOrElse(100)
    val cachePath = args.getOption("--cache=").getOrElse("cache")

    println("My options are:")
    println(s"  threads=$nThreads")
    println(s"  runs=$nRuns")
    println(s"  cache='$cachePath'")

    val runner = new Runner(nThreads)
    try {
      val semoConfigurations = Seq(100, 200, 300, 400, 500, 1000).flatMap(n => Seq(
        Config(new OneZeroMax(n), getSimpleSEMO),
        Config(new OneZeroMax(n), getCrowdingSEMO),
        Config(new OneZeroMax(n), getFertilitySEMO),
        Config(new LeadingOnesTrailingZeros(n), getSimpleSEMO),
        Config(new LeadingOnesTrailingZeros(n), getCrowdingSEMO),
        Config(new LeadingOnesTrailingZeros(n), getFertilitySEMO)
      ))

      val semoByProblem = byProblem(semoConfigurations)

      val plotter = new Plotter(p => {
        val name = p.name
        val open = name.indexOf('(')
        val next = name.indexWhere(!_.isDigit, open + 1)
        val title0 = name.substring(0, open)
        val title = title0.flatMap(ch => if (ch.isLetter) Some(ch) else None)
        (title, name.substring(open + 1, next).toDouble)
      })

      for ((problemName, configs) <- semoByProblem) {
        println(s"$problemName:")
        for (config <- configs) {
          val algorithm = config.algorithm
          val problem = config.problem
          val stats = runner.compute(cachePath, algorithm, problem, nRuns).transpose.map(d => new Statistics(d))
          val names = algorithm.metrics
          println(s"  ${algorithm.name}:")
          for ((stat, name) <- (stats, names).zipped) {
            println(s"    $name: ${stat.everything}")
          }
          val fevMetricIndex = names.indexOf("Fitness evaluations")
          plotter.append(algorithm, problem, stats(fevMetricIndex))
        }
      }
    } finally {
      runner.close()
    }
  }
}
