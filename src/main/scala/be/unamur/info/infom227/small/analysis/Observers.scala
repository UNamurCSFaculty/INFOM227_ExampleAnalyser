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

case class Row[T <: Lattice[T]](programPoint: ProgramPoint, worklist: Set[ProgramPoint], beforeVariables: Map[String, T], afterVariables: Map[String, T])

case class Table[T <: Lattice[T]](rows: mutable.ListBuffer[Row[T]] = mutable.ListBuffer.empty[Row[T]], bottomSymbol: String = ""):
  private def center(string: String, width: Int): String = {
    val padding = math.max(0, width - string.length)
    val left = padding / 2
    val right = padding - left
    " " * left + string + " " * right
  }

  private def headerDelimiter(width: Int): String = {
    ":" ++ "-" * (width - 2) ++ ":"
  }

  private def rowToString(pp: String, worklist: String, before: List[String], after: List[String], delim: String): String = {
    s"$delim$pp$delim$worklist$delim${before.mkString(delim)}$delim${after.mkString(delim)}$delim\n"
  }

  override def toString: String = {
    val variables = rows.foldLeft(Set.empty[String]) { (acc, row) =>
      acc.union(row.beforeVariables.keys.toSet).union(row.afterVariables.keys.toSet)
    }.toList
    val header = List((" PP ", " WL ", variables.map { variable => f" Φ($variable) " }, variables.map { variable => f" res($variable) " }))
    val stringTable = rows.foldLeft(header) { (acc, row) =>
      acc :+ (
        s" ${row.programPoint.toString} ",
        s" ${row.worklist.mkString(",")} ",
        variables.map { variable =>
          row.beforeVariables.get(variable) match {
            case Some(value) => s" ${value.toString} "
            case None => bottomSymbol
          }
        },
        variables.map { variable =>
          row.afterVariables.get(variable) match {
            case Some(value) => s" ${value.toString} "
            case None => bottomSymbol
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

    val builder = new StringBuilder()
    for (((pp, wl, before, after), i) <- stringTable.zipWithIndex) {
      builder.append(rowToString(
        center(pp, ppSize),
        center(wl, wlSize),
        before.zip(beforeSize).map { (b, s) => center(b, s) },
        after.zip(afterSize).map { (a, s) => center(a, s) },
        "|"
      ))
      if (i == 0) {
        builder.append(rowToString(
          headerDelimiter(ppSize),
          headerDelimiter(wlSize),
          beforeSize.map(headerDelimiter),
          afterSize.map(headerDelimiter),
          "|"
        ))
      }
    }
    builder.toString
  }

case class TableObserver[T <: Lattice[T], S <: State[T]](var beforeAbstractState: Option[S] = None, table: Table[T] = Table[T]()) extends AnalysisObserver[ProgramPoint, S, AnalysisState[S]]:
  override def beforeNodeAnalysis(analysisState: AnalysisState[S], worklist: mutable.Set[ProgramPoint], node: ProgramPoint): Unit = {
    beforeAbstractState = analysisState.abstractStates.get(node)
  }

  override def afterNodeAnalysis(analysisState: AnalysisState[S], abstractState: S, worklist: mutable.Set[ProgramPoint], node: ProgramPoint): Unit = {
    table.rows += Row(
      node,
      worklist.toSet,
      beforeAbstractState match {
        case Some(state) => state.variables
        case None => Map.empty
      },
      abstractState.variables
    )
  }
