package onell

import java.nio.file.{Files, Paths}
import java.util.{Arrays, Locale}
import java.util.concurrent.{Callable, Executors, ThreadLocalRandom}
import java.util.function.Consumer

import onell.algorithms.{OnePlusLambdaLambdaGA, OnePlusOneEA}
import onell.problems.OneMax

import scala.language.implicitConversions
import scala.collection.JavaConverters._
import scala.util.Random

/**
  * The main class which runs experiments.
  */
object Main {
  def main(args: Array[String]): Unit = {
    Locale.setDefault(Locale.US)
    val runner = new Runner
    try {
      for (problemGen <- Seq((n: Int) => new OneMax(n))) {
        for (n <- Seq(10, 100, 1000, 10000, 100000, 1000000)) {
          val problem = problemGen(n)
          println(s"${problem.name}:")
          for (algorithm <- Seq(
            OnePlusOneEA,
            new OnePlusLambdaLambdaGA(),
            new OnePlusLambdaLambdaGA(1, "1", 2 * math.log(n + 1), "2 ln n")
          )) {
            val stats = runner.compute("cache", algorithm, problem, 100).transpose.map(d => new Statistics(d))
            val names = algorithm.metrics
            println(s"  ${algorithm.name}:")
            for ((stat, name) <- (stats, names).zipped) {
              println(s"    $name: ${stat.everything}")
            }
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
  implicit val rng = new Random(ThreadLocalRandom.current())

  class Runner {
    val service = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())

    def close(): Unit = {
      service.shutdown()
    }

    def runInParallel(algorithm: Algorithm, problem: MutationAwarePseudoBooleanProblem, times: Int): Seq[Seq[Long]] = {
      val threadLocalProblem = new ThreadLocal[MutationAwarePseudoBooleanProblem] {
        override protected def initialValue(): MutationAwarePseudoBooleanProblem = problem.clone
      }
      val timeStart = System.nanoTime()
      val tasks = Array.fill[Callable[Seq[Long]]](times)(() => algorithm.solve(threadLocalProblem.get()))
      val rv = service.invokeAll(Arrays.asList(tasks :_*)).asScala.map(_.get()).toIndexedSeq
      val timeDone = (System.nanoTime() - timeStart) / 1e9
      println(s"  [$times runs done in $timeDone s]")
      rv
    }

    def compute(
      cacheRoot: String,
      algorithm: Algorithm,
      problem: MutationAwarePseudoBooleanProblem,
      times: Int
    ): Seq[Seq[Long]] = {
      val path = Paths.get(cacheRoot, algorithm.name, problem.name)
      val metricCount = algorithm.metrics.size
      val previousResults = if (Files.exists(path)) {
        val lines = Files.lines(path)
        val okResults = Seq.newBuilder[Seq[Long]]
        lines.forEach((t: String) => try {
          val longs = t.split(" ").filter(_.nonEmpty).map(_.toLong)
          if (longs.length == metricCount) {
            okResults += longs.toIndexedSeq
          } else {
            println(s"The number of entries in the string '$t' is wrong: expected $metricCount found ${longs.length}")
          }
        } catch {
          case e: Throwable => println(s"An exception of type ${e.getClass} thrown when processing string '$t'")
        })
        okResults.result()
      } else Seq()
      if (previousResults.size < times) {
        val result = previousResults ++ runInParallel(algorithm, problem, times - previousResults.size)
        val resultSorted = result.sortBy(_(0))
        Files.createDirectories(path.getParent)
        Files.write(path, resultSorted.map(_.mkString(" ")).asJava)
        resultSorted
      } else {
        previousResults
      }
    }
  }

  class Statistics(data: Seq[Long]) {
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

    lazy val min: Long = sortedData.head
    lazy val max: Long = sortedData.last
    lazy val std: Double = {
      val avg = mean
      val sumSqDiff = sortedData.view.map(v => (v - avg) * (v - avg)).sum
      math.sqrt(sumSqDiff  / (sortedData.size - 1))
    }
    lazy val mean: Double = sortedData.sum.toDouble / sortedData.size
    lazy val median: Double = percentile(0.5)
    lazy val interQuartile: Double = percentile(0.75) - percentile(0.25)

    def everything: String = {
      f"{$mean%.02f \u00b1 $std%.02f, med = $median%.02f, iqr = $interQuartile%.02f}"
    }
  }
}
