package onell

import java.util.Locale

import onell.algorithms.{GlobalSEMO, OnePlusLambdaLambdaGA, OnePlusOneEA}
import onell.problems.{LeadingOnesTrailingZeros, OneMax, OneZeroMax, Random3CNF}
import onell.util.RunHelpers._

import scala.language.implicitConversions

/**
  * The main class which runs experiments.
  */
object Main {
  def getOneMax(n: Int)     = new OneMax(n)
  def getRandom3CNF(n: Int) = new Random3CNF(n, (4 * n * math.log(n)).toInt)

  def getOnePlusOneEA(n: Int) = OnePlusOneEA
  def getOnePlusLLN(n: Int)   = new OnePlusLambdaLambdaGA()
  def getOnePlusLLLog(n: Int) = new OnePlusLambdaLambdaGA(1, "1", 2 * math.log(n + 1), "2 ln n")
  def getOnePlusLLLogRoot2(n: Int) = new OnePlusLambdaLambdaGA(1, "1", math.pow(2 * math.log(n + 1), 0.5), "pow(2 ln n, 0.5)")
  def getOnePlusLLLogRoot4(n: Int) = new OnePlusLambdaLambdaGA(1, "1", math.pow(2 * math.log(n + 1), 0.25), "pow(2 ln n, 0.25)")
  def getOnePlusLLx(n: Int, x: Int) = new OnePlusLambdaLambdaGA(1, "1", x, x.toString)

  def getSimpleSEMO    = new GlobalSEMO with GlobalSEMO.Niching.None with GlobalSEMO.Selection.Uniform
  def getCrowdingSEMO  = new GlobalSEMO with GlobalSEMO.Niching.None with GlobalSEMO.Selection.Crowding
  def getFertilitySEMO = new GlobalSEMO with GlobalSEMO.Niching.None with GlobalSEMO.Selection.Fertility

  def main(args: Array[String]): Unit = {
    Locale.setDefault(Locale.US)

    val runner = new Runner
    try {
      val oneLLConfigurations = {
        (4 to 20).map(1 << _).flatMap(n => Seq(
          Config(getOneMax(n), getOnePlusOneEA(n)),
          Config(getOneMax(n), getOnePlusLLN(n)),
          Config(getOneMax(n), getOnePlusLLLog(n)),
          Config(getOneMax(n), getOnePlusLLLogRoot2(n)),
          Config(getOneMax(n), getOnePlusLLLogRoot4(n))
        ))
      } ++ {
        (7 to 16).map(1 << _).flatMap(n => Seq(
          Config(getRandom3CNF(n), getOnePlusOneEA(n)),
          Config(getRandom3CNF(n), getOnePlusLLLog(n)),
          Config(getOneMax(n), getOnePlusLLLogRoot2(n)),
          Config(getOneMax(n), getOnePlusLLLogRoot4(n))
        ))
      } ++ {
        (7 to 12).flatMap(n => Seq(
          Config(getRandom3CNF(n), getOnePlusLLN(n))
        ))
      }

      val fixedOneLLConfigurations = for {
        x <- 2 to 20
        np <- 7 to 16
        n = 1 << np
        p <- Seq(getOneMax(n), getRandom3CNF(n))
      } yield Config(p, getOnePlusLLx(n, x))

      val semoConfigurations = Seq(100, 200, 300, 400, 500, 1000).flatMap(n => Seq(
        Config(new OneZeroMax(n), getSimpleSEMO),
        Config(new OneZeroMax(n), getCrowdingSEMO),
        Config(new OneZeroMax(n), getFertilitySEMO),
        Config(new LeadingOnesTrailingZeros(n), getSimpleSEMO),
        Config(new LeadingOnesTrailingZeros(n), getCrowdingSEMO),
        Config(new LeadingOnesTrailingZeros(n), getFertilitySEMO)
      ))

      val oneLLByProblem = byProblem(oneLLConfigurations)
      val fixedOneLLByProblem = byProblem(fixedOneLLConfigurations)
      val semoByProblem = byProblem(semoConfigurations)

      for ((problemName, configs) <- oneLLByProblem ++ fixedOneLLByProblem ++ semoByProblem) {
        println(s"$problemName:")
        for (config <- configs) {
          val algorithm = config.algorithm
          val problem = config.problem
          val stats = runner.compute("cache", algorithm, problem, 100).transpose.map(d => new Statistics(d))
          val names = algorithm.metrics
          println(s"  ${algorithm.name}:")
          for ((stat, name) <- (stats, names).zipped) {
            println(s"    $name: ${stat.everything}")
          }
        }
      }
    } finally {
      runner.close()
    }
  }
}
