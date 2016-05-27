package onell

import java.nio.file.{Files, Paths}
import java.util.concurrent.{Callable, Executors, ThreadLocalRandom}
import java.util.function.Consumer
import java.util.{Arrays, Locale, Random}

import onell.algorithms.{OnePlusLambdaLambdaGA, OnePlusOneEA}
import onell.problems.{OneMax, Random3CNF}

import scala.collection.JavaConverters._
import scala.language.implicitConversions

/**
  * The main class which runs experiments.
  */
object Main {
  def main(args: Array[String]): Unit = {
    Locale.setDefault(Locale.US)
    val runner = new Runner
    try {
      def getOneMax(n: Int)     = new OneMax(n)
      def getRandom3CNF(n: Int) = new Random3CNF(n, (4 * n * math.log(n)).toInt)

      def getOnePlusOneEA(n: Int) = OnePlusOneEA
      def getOnePlusLLN(n: Int)   = new OnePlusLambdaLambdaGA()
      def getOnePlusLLlog(n: Int) = new OnePlusLambdaLambdaGA(1, "1", 2 * math.log(n + 1), "2 ln n")

      val configurations = {
        Seq(10, 100, 1000, 10000, 100000, 1000000).flatMap(n => Seq(
          (getOneMax(n), getOnePlusOneEA(n)),
          (getOneMax(n), getOnePlusLLN(n)),
          (getOneMax(n), getOnePlusLLlog(n))
        ))
      } ++ {
        Seq(100, 300, 1000, 3000, 10000, 30000, 100000).flatMap(n => Seq(
          (getRandom3CNF(n), getOnePlusOneEA(n)),
          (getRandom3CNF(n), getOnePlusLLlog(n))
        ))
      } ++ {
        Seq(100, 300, 1000, 3000).flatMap(n => Seq(
          (getRandom3CNF(n), getOnePlusLLN(n))
        ))
      }

      val byProblem = configurations.groupBy(_._1.name).mapValues(_.sortBy(_._2.name)).toIndexedSeq.sortBy(_._1)

      for ((problemName, configs) <- byProblem) {
        println(s"$problemName:")
        for ((problem, algorithm) <- configs) {
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

  implicit def functionToCallable[T](function: () => T): Callable[T] = new Callable[T] {
    override def call(): T = function()
  }
  implicit def functionToConsumer[T](function: T => Any): Consumer[T] = new Consumer[T] {
    override def accept(t: T): Unit = function(t)
  }
  implicit def rng: Random = ThreadLocalRandom.current()

  class Runner {
    val service = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())

    def close(): Unit = {
      service.shutdown()
    }

    def runInParallel[F](algorithm: Algorithm[F], problem: MutationAwarePseudoBooleanProblem[F], times: Int): Seq[Seq[Double]] = {
      val threadLocalProblem = new ThreadLocal[MutationAwarePseudoBooleanProblem[F]] {
        override protected def initialValue(): MutationAwarePseudoBooleanProblem[F] = problem.copy
      }
      val timeStart = System.nanoTime()
      val tasks = Array.fill[Callable[Seq[Double]]](times)(() => algorithm.solve(threadLocalProblem.get()))
      val rv = service.invokeAll(Arrays.asList(tasks :_*)).asScala.map(_.get()).toIndexedSeq
      val timeDone = (System.nanoTime() - timeStart) / 1e9
      println(s"  [$times runs done in $timeDone s]")
      rv
    }

    def compute[F](
      cacheRoot: String,
      algorithm: Algorithm[F],
      problem: MutationAwarePseudoBooleanProblem[F],
      times: Int
    ): Seq[Seq[Double]] = {
      val path = Paths.get(cacheRoot, algorithm.name, problem.name)
      val metricCount = algorithm.metrics.size
      val previousResults = if (Files.exists(path)) {
        val lines = Files.lines(path)
        val okResults = Seq.newBuilder[Seq[Double]]
        var revisionString: String = null
        lines.forEach((t: String) => try {
          if (revisionString == null) {
            revisionString = t
          } else {
            val values = t.split(" ").filter(_.nonEmpty).map(_.toDouble)
            if (values.length == metricCount) {
              okResults += values.toIndexedSeq
            }
          }
        } catch {
          case e: Throwable => println(s"An exception of type ${e.getClass} thrown when processing string '$t'")
        })
        if (revisionString != algorithm.revision) Seq() else okResults.result()
      } else Seq()
      if (previousResults.size < times) {
        val result = previousResults ++ runInParallel(algorithm, problem, times - previousResults.size)
        val resultSorted = result.sortBy(_(0))
        Files.createDirectories(path.getParent)
        Files.write(path, (algorithm.revision +: resultSorted.map(_.mkString(" "))).asJava)
        resultSorted
      } else {
        previousResults
      }
    }
  }

  class Statistics(data: Seq[Double]) {
    val sortedData = data.toIndexedSeq.sorted

    def percentile(ratio: Double): Double = {
      val index = (sortedData.size - 1) * ratio
      val indexD = math.floor(index).toInt
      val indexU = math.ceil(index).toInt
      if (indexD == indexU) {
        sortedData(indexD)
      } else {
        sortedData(indexD) * (indexU - index) + sortedData(indexU) * (index - indexD)
      }
    }

    lazy val min: Double = sortedData.head
    lazy val max: Double = sortedData.last
    lazy val std: Double = {
      val avg = mean
      val sumSqDiff = sortedData.view.map(v => (v - avg) * (v - avg)).sum
      math.sqrt(sumSqDiff  / (sortedData.size - 1))
    }
    lazy val mean: Double = sortedData.sum / sortedData.size
    lazy val median: Double = percentile(0.5)
    lazy val interQuartile: Double = percentile(0.75) - percentile(0.25)

    def everything: String = {
      f"{$mean%.02f \u00b1 $std%.02f, med = $median%.02f, iqr = $interQuartile%.02f}"
    }
  }
}
