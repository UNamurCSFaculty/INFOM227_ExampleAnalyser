package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.cfg.ProgramPoint

import scala.collection.mutable

trait AnalysisObserver[N, S, A] {
  def beforeAnalysis(analysisState: A, worklist: mutable.Set[N]): Unit = {}

  def beforeIteration(analysisState: A, worklist: mutable.Set[N]): Unit = {}

  def beforeNodeAnalysis(analysisState: A, worklist: mutable.Set[N], node: N): Unit = {}

  def afterNodeAnalysis(analysisState: A, abstractState: S, worklist: mutable.Set[N], node: N): Unit = {}

  def afterIteration(analysisState: A, worklist: mutable.Set[N]): Unit = {}

  def afterAnalysis(analysisState: A, worklist: mutable.Set[N]): Unit = {}
}

class DummyObserver[N, S, A] extends AnalysisObserver[N, S, A]

case class TableRow[T](programPoint: ProgramPoint, worklist: Set[ProgramPoint], beforeAbstractState: AbstractState[T], afterAbstractState: AbstractState[T])

case class TableObserver[T](var beforeAbstractState: Option[AbstractState[T]] = None, table: mutable.ListBuffer[TableRow[T]] = mutable.ListBuffer[TableRow[T]]()) extends AnalysisObserver[ProgramPoint, AbstractState[T], AnalysisState[T]]:
  override def beforeNodeAnalysis(analysisState: AnalysisState[T], worklist: mutable.Set[ProgramPoint], node: ProgramPoint): Unit = {
    beforeAbstractState = analysisState.abstractStates.get(node)
  }

  override def afterNodeAnalysis(analysisState: AnalysisState[T], abstractState: AbstractState[T], worklist: mutable.Set[ProgramPoint], node: ProgramPoint): Unit = {
    table += TableRow(node, worklist.toSet, beforeAbstractState.getOrElse(AbstractState()), abstractState)
  }

  private def printRow(pp: String, worklist: String, before: List[String], after: List[String], delim: String): Unit = {
    println(s"$delim$pp$delim$worklist$delim${before.mkString(delim)}$delim${after.mkString(delim)}$delim")
  }

  private def center(string: String, width: Int): String = {
    val padding = math.max(0, width - string.length)
    val left = padding / 2
    val right = padding - left
    " " * left + string + " " * right
  }

  private def headerDelimiter(width: Int): String = {
    ":" ++ "-" * (width - 2) ++ ":"
  }

  override def afterAnalysis(analysisState: AnalysisState[T], worklist: mutable.Set[ProgramPoint]): Unit = {
    val variables = table.foldLeft(Set.empty[String]) { (acc, row) =>
      acc.union(row.beforeAbstractState.variables.keys.toSet).union(row.afterAbstractState.variables.keys.toSet)
    }.toList
    val header = List((" PP ", " WL ", variables.map { variable => f" Φ($variable) " }, variables.map { variable => f" res($variable) " }))
    val stringTable = table.foldLeft(header) { (acc, row) =>
      acc :+ (
        s" ${row.programPoint.toString} ",
        s" ${row.worklist.mkString(",")} ",
        variables.map { variable =>
          row.beforeAbstractState.variables.get(variable) match {
            case Some(value) => s" ${value.toString} "
            case None => "⊥"
          }
        },
        variables.map { variable =>
          row.afterAbstractState.variables.get(variable) match {
            case Some(value) => s" ${value.toString} "
            case None => "⊥"
          }
        }
      )
    }
    val (ppSize, wlSize, beforeSize, afterSize) = stringTable.foldLeft((0, 0, variables.map(_ => 0), variables.map(_ => 0))) { case ((ppAccSize, wlAccSize, beforeAccSize, afterAccSize), (pp, wl, before, after)) =>
      (
        math.max(ppAccSize, pp.length),
        math.max(wlAccSize, wl.length),
        before.zip(beforeAccSize).map { (b, acc) => math.max(acc, b.length) },
        after.zip(afterAccSize).map { (a, acc) => math.max(acc, a.length) }
      )
    }

    for (((pp, wl, before, after), i) <- stringTable.zipWithIndex) {
      printRow(
        center(pp, ppSize),
        center(wl, wlSize),
        before.zip(beforeSize).map { (b, s) => center(b, s) },
        after.zip(afterSize).map { (a, s) => center(a, s) },
        "|"
      )
      if (i == 0) {
        printRow(headerDelimiter(ppSize), headerDelimiter(wlSize), beforeSize.map(headerDelimiter), afterSize.map(headerDelimiter), "|")
      }
    }
  }
