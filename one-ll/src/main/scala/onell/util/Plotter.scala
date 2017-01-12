package onell.util

import java.io.{IOException, PrintWriter}

import scala.collection.mutable.{HashMap => MuHashMap, TreeMap => MuTreeMap}
import onell.util.RunHelpers.Statistics
import onell.{Algorithm, MutationAwarePseudoBooleanProblem}

/**
  * A facade for the plotting capabilities.
  */
class Plotter(classifier: MutationAwarePseudoBooleanProblem[_] => (String, Double)) {
  private final val map = new MuHashMap[String, MuTreeMap[String, MuTreeMap[Double, Statistics]]]()

  def append[F](algorithm: Algorithm[F], problem: MutationAwarePseudoBooleanProblem[F], stats: Statistics): Unit = {
    val (token, x) = classifier(problem)
    map.getOrElseUpdate(token, new MuTreeMap()(RunHelpers.numberTokenSorting)).getOrElseUpdate(algorithm.name, new MuTreeMap()) += x -> stats
  }

  private def intToString(value: Int): String = {
    if (value < 26) {
      ('A' + value).toChar.toString
    } else {
      val ch = ('A' + value % 26).toChar.toString
      intToString(value / 26) + ch
    }
  }
  private val intToColor0 = IndexedSeq(
    "red", "green", "blue", "Apricot", "Bittersweet",
    "DarkOrchid", "JungleGreen", "LimeGreen", "Mulberry",
    "OrangeRed", "Peach", "RedOrange", "RoyalPurple",
    "Sepia", "SpringGreen", "Rhodamine", "VioletRed",
    "YellowOrange", "Magenta", "NavyBlue", "RubineRed", "black"
  )
  private def intToColor(index: Int) = intToColor0(index % intToColor0.size)

  def writeAllTikZPlots(filename: String, filter: String => Boolean): Unit = {
    try {
      val pw = new PrintWriter(filename)
      val delta = 0.05
      for ((clazz, plots) <- map if filter(clazz)) {
        pw.println(s"\\newcommand{\\iqrPlot$clazz}[2]{")
        pw.println("  \\begin{tikzpicture}")
        pw.println("    \\begin{loglogaxis}[xlabel=Problem size, ylabel=Evaluations, " +
          "width=#1, height=#2, legend pos=outer north east]")
        for (((algo, plot), index) <- plots.toIndexedSeq.zipWithIndex) {
          val tag = intToString(index)
          pw.print(s"      \\addplot[color = ${intToColor(index)}, name path=s$tag, forget plot] coordinates {")
          for ((x, stat) <- plot) {
            pw.print(s"($x, ${stat.percentile(0.5 - delta) / x})")
          }
          pw.println("};")
          pw.print(s"      \\addplot[color = ${intToColor(index)}, name path=e$tag, forget plot] coordinates {")
          for ((x, stat) <- plot) {
            pw.print(s"($x, ${stat.percentile(0.5 + delta) / x})")
          }
          pw.println("};")
          pw.println(s"      \\addplot[fill = ${intToColor(index)}, area legend] fill between[of = s$tag and e$tag];")
          pw.println(s"      \\addlegendentry{$algo};")
        }
        pw.println("    \\end{loglogaxis}")
        pw.println("  \\end{tikzpicture}")
        pw.println("}")
      }
      pw.close()
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }
}
