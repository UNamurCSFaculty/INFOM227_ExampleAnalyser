package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast.{ArithmeticBinaryOperation, ArithmeticBinaryOperator, ArithmeticConstant, AssignStatement, BooleanConstant, BooleanExpression, BooleanNegOperation, EqualComparisonOperator, Expression, FunctionCall, IntegerComparisonOperation, IntegerComparisonOperator, Statement, Variable}
import be.unamur.info.infom227.small.cfg.{Cfg, ProgramPoint}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

enum ZeroAnalysisAbstractValue extends Lattice[ZeroAnalysisAbstractValue]:
  case Unknown
  case Zero
  case NonZero
  case Bottom

  override def join(other: ZeroAnalysisAbstractValue): ZeroAnalysisAbstractValue = {
    (this, other) match {
      case (ZeroAnalysisAbstractValue.Bottom, _) => other
      case (_, ZeroAnalysisAbstractValue.Bottom) => this
      case (ZeroAnalysisAbstractValue.Zero, ZeroAnalysisAbstractValue.Zero) => ZeroAnalysisAbstractValue.Zero
      case (ZeroAnalysisAbstractValue.NonZero, ZeroAnalysisAbstractValue.NonZero) => ZeroAnalysisAbstractValue.NonZero
      case _ => ZeroAnalysisAbstractValue.Unknown
    }
  }

  override def meet(other: ZeroAnalysisAbstractValue): ZeroAnalysisAbstractValue = {
    (this, other) match {
      case (ZeroAnalysisAbstractValue.Unknown, _) => other
      case (_, ZeroAnalysisAbstractValue.Unknown) => this
      case (ZeroAnalysisAbstractValue.Zero, ZeroAnalysisAbstractValue.Zero) => ZeroAnalysisAbstractValue.Zero
      case (ZeroAnalysisAbstractValue.NonZero, ZeroAnalysisAbstractValue.NonZero) => ZeroAnalysisAbstractValue.NonZero
      case _ => ZeroAnalysisAbstractValue.Bottom
    }
  }

  override def toString: String = {
    this match {
      case ZeroAnalysisAbstractValue.Unknown => "U"
      case ZeroAnalysisAbstractValue.Zero => "Z"
      case ZeroAnalysisAbstractValue.NonZero => "NZ"
      case ZeroAnalysisAbstractValue.Bottom => "⊥"
    }
  }

case class ZeroAnalysis(cfg: Cfg) extends GraphAnalyser[ProgramPoint, AbstractState[ZeroAnalysisAbstractValue], AnalysisState[ZeroAnalysisAbstractValue]]:
  @tailrec
  private def analyseStatement(abstractState: AbstractState[ZeroAnalysisAbstractValue], statement: Statement): AbstractState[ZeroAnalysisAbstractValue] = {
    statement match {
      case AssignStatement(lineNumber, variable, expression) =>
        expression match {
          case ArithmeticConstant(c) =>
            if (c == 0) {
              abstractState(variable -> ZeroAnalysisAbstractValue.Zero)
            } else {
              abstractState(variable -> ZeroAnalysisAbstractValue.NonZero)
            }
          case Variable(y) =>
            abstractState(variable -> abstractState(y))
          case ArithmeticBinaryOperation(ArithmeticConstant(c), ArithmeticBinaryOperator.Add, ArithmeticConstant(d)) =>
            if (c == -d) {
              abstractState(variable -> ZeroAnalysisAbstractValue.Zero)
            } else {
              abstractState(variable -> ZeroAnalysisAbstractValue.Unknown)
            }
          case ArithmeticBinaryOperation(Variable(y), ArithmeticBinaryOperator.Add, Variable(z)) =>
            if (abstractState(y) == ZeroAnalysisAbstractValue.Zero && abstractState(z) == ZeroAnalysisAbstractValue.Zero) {
              abstractState(variable -> ZeroAnalysisAbstractValue.Zero)
            } else {
              abstractState(variable -> ZeroAnalysisAbstractValue.Unknown)
            }
          case ArithmeticBinaryOperation(Variable(y), ArithmeticBinaryOperator.Add, ArithmeticConstant(c)) =>
            if (abstractState(y) == ZeroAnalysisAbstractValue.Zero && c == 0) {
              abstractState(variable -> ZeroAnalysisAbstractValue.Zero)
            } else if (abstractState(y) == ZeroAnalysisAbstractValue.Zero && c != 0) {
              abstractState(variable -> ZeroAnalysisAbstractValue.NonZero)
            } else if (abstractState(y) == ZeroAnalysisAbstractValue.NonZero && c == 0) {
              abstractState(variable -> ZeroAnalysisAbstractValue.NonZero)
            } else {
              abstractState(variable -> ZeroAnalysisAbstractValue.Unknown)
            }
          case ArithmeticBinaryOperation(ArithmeticConstant(c), ArithmeticBinaryOperator.Add, Variable(y)) =>
            analyseStatement(abstractState, AssignStatement(lineNumber, variable, ArithmeticBinaryOperation(Variable(y), ArithmeticBinaryOperator.Add, ArithmeticConstant(c))))
          case _ =>
            abstractState(variable -> ZeroAnalysisAbstractValue.Unknown)
        }
      case _ =>
        abstractState
    }
  }

  @tailrec
  private def conditionUpdate(abstractState: AbstractState[ZeroAnalysisAbstractValue], condition: BooleanExpression): Option[AbstractState[ZeroAnalysisAbstractValue]] = {
    condition match {
      case BooleanConstant(true) =>
        Some(abstractState)
      case BooleanConstant(false) =>
        None
      case IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Lt, ArithmeticConstant(c)) =>
        val metAbstractValue = abstractState(y).meet(ZeroAnalysisAbstractValue.NonZero)

        if (c <= 0 && metAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState(y -> metAbstractValue))
        } else if (c > 0) {
          Some(abstractState)
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), IntegerComparisonOperator.Lt, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Gt, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Gt, ArithmeticConstant(c)) =>
        val metAbstractValue = abstractState(y).meet(ZeroAnalysisAbstractValue.NonZero)

        if (c >= 0 && metAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState(y -> metAbstractValue))
        } else if (c < 0) {
          Some(abstractState)
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), IntegerComparisonOperator.Gt, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Lt, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Lte, ArithmeticConstant(c)) =>
        val metAbstractValue = abstractState(y).meet(ZeroAnalysisAbstractValue.NonZero)

        if (c < 0 && metAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState(y -> metAbstractValue))
        } else if (c >= 0) {
          Some(abstractState)
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), IntegerComparisonOperator.Lte, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Gte, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Gte, ArithmeticConstant(c)) =>
        val metAbstractValue = abstractState(y).meet(ZeroAnalysisAbstractValue.NonZero)

        if (c > 0 && metAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState(y -> metAbstractValue))
        } else if (c <= 0) {
          Some(abstractState)
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), IntegerComparisonOperator.Gte, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Lte, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), EqualComparisonOperator.Eq, ArithmeticConstant(c)) =>
        val metZeroAbstractValue = abstractState(y).meet(ZeroAnalysisAbstractValue.Zero)
        val metNonZeroAbstractValue = abstractState(y).meet(ZeroAnalysisAbstractValue.NonZero)

        if (c == 0 && metZeroAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState(y -> metZeroAbstractValue))
        } else if (c != 0 && metNonZeroAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState(y -> metNonZeroAbstractValue))
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), EqualComparisonOperator.Eq, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), EqualComparisonOperator.Eq, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), EqualComparisonOperator.Ne, ArithmeticConstant(c)) =>
        val metNonZeroAbstractValue = abstractState(y).meet(ZeroAnalysisAbstractValue.NonZero)

        if (c == 0 && metNonZeroAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState(y -> metNonZeroAbstractValue))
        } else if (c != 0) {
          Some(abstractState)
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), EqualComparisonOperator.Ne, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), EqualComparisonOperator.Ne, ArithmeticConstant(c)))
      case _ =>
        Some(abstractState)
    }
  }

  override def entryNodes: Set[ProgramPoint] = cfg.entryPoints

  override def nextNodes(abstractState: AbstractState[ZeroAnalysisAbstractValue], node: ProgramPoint): Try[Set[ProgramPoint]] = Success(cfg.successors(node))

  override def initialiseAnalysisState(): Try[AnalysisState[ZeroAnalysisAbstractValue]] = Success(AnalysisState(mutable.Map(ProgramPoint.EntryPoint -> AbstractState(cfg.parameters.map { parameter => parameter -> ZeroAnalysisAbstractValue.Unknown }.toMap))))

  override def analyseNode(analysisState: AnalysisState[ZeroAnalysisAbstractValue], node: ProgramPoint): Try[AbstractState[ZeroAnalysisAbstractValue]] = {
    val abstractState = analysisState.abstractStates.getOrElse(node, AbstractState())

    node match {
      case ProgramPoint.StatementPoint(statement) => Try(analyseStatement(abstractState, statement))
      case _ => Success(abstractState)
    }
  }

  override def updateAbstractState(analysisState: AnalysisState[ZeroAnalysisAbstractValue], from: ProgramPoint, to: ProgramPoint, abstractState: AbstractState[ZeroAnalysisAbstractValue]): Try[Option[AbstractState[ZeroAnalysisAbstractValue]]] =
    cfg.condition(from, to) match {
      case Some(condition) => Try(conditionUpdate(abstractState, condition))
      case _ => Failure(new RuntimeException("condition should always exist"))
    }

  override def getAbstractState(analysisState: AnalysisState[ZeroAnalysisAbstractValue], node: ProgramPoint): Try[Option[AbstractState[ZeroAnalysisAbstractValue]]] = Success(analysisState.abstractStates.get(node))

  override def setAbstractState(analysisState: AnalysisState[ZeroAnalysisAbstractValue], node: ProgramPoint, abstractState: AbstractState[ZeroAnalysisAbstractValue]): Try[Unit] = {
    analysisState.abstractStates.addOne(node -> abstractState)
    Success(())
  }

  override def merge(analysisState: AnalysisState[ZeroAnalysisAbstractValue], node: ProgramPoint, left: AbstractState[ZeroAnalysisAbstractValue], right: AbstractState[ZeroAnalysisAbstractValue]): Try[AbstractState[ZeroAnalysisAbstractValue]] = {
    Success(AbstractState(left.variables.foldLeft(right.variables) { (acc, entry) =>
      val (name, newAbstractValue) = entry
      val mergedValue = acc.get(name) match {
        case Some(currentAbstractValue) => newAbstractValue.join(currentAbstractValue)
        case None => newAbstractValue
      }
      acc + (name -> mergedValue)
    }))
  }

case class ZeroAnalysisRow(programPoint: ProgramPoint, worklist: Set[ProgramPoint], beforeAbstractState: AbstractState[ZeroAnalysisAbstractValue], afterAbstractState: AbstractState[ZeroAnalysisAbstractValue])

case class ZeroAnalysisObserver(var beforeAbstractState: Option[AbstractState[ZeroAnalysisAbstractValue]] = None, table: mutable.ListBuffer[ZeroAnalysisRow] = mutable.ListBuffer()) extends AnalysisObserver[ProgramPoint, AbstractState[ZeroAnalysisAbstractValue], AnalysisState[ZeroAnalysisAbstractValue]]:
  override def beforeNodeAnalysis(analysisState: AnalysisState[ZeroAnalysisAbstractValue], worklist: mutable.Set[ProgramPoint], node: ProgramPoint): Unit = {
    beforeAbstractState = analysisState.abstractStates.get(node)
  }

  override def afterNodeAnalysis(analysisState: AnalysisState[ZeroAnalysisAbstractValue], abstractState: AbstractState[ZeroAnalysisAbstractValue], worklist: mutable.Set[ProgramPoint], node: ProgramPoint): Unit = {
    table += ZeroAnalysisRow(node, worklist.toSet, beforeAbstractState.getOrElse(AbstractState()), abstractState)
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

  override def afterAnalysis(analysisState: AnalysisState[ZeroAnalysisAbstractValue], worklist: mutable.Set[ProgramPoint]): Unit = {
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

def zeroAnalysis(cfgs: Map[String, Cfg], observer: AnalysisObserver[ProgramPoint, AbstractState[ZeroAnalysisAbstractValue], AnalysisState[ZeroAnalysisAbstractValue]]): Try[Map[String, AnalysisState[ZeroAnalysisAbstractValue]]] = {
  cfgs.foldLeft(Try(Map.empty[String, AnalysisState[ZeroAnalysisAbstractValue]])) { (acc, entry) =>
    for {
      results <- acc
      (name, cfg) = entry
      analysisState <- analysis[ProgramPoint, AbstractState[ZeroAnalysisAbstractValue], AnalysisState[ZeroAnalysisAbstractValue], ZeroAnalysis, AnalysisObserver[ProgramPoint, AbstractState[ZeroAnalysisAbstractValue], AnalysisState[ZeroAnalysisAbstractValue]]](ZeroAnalysis(cfg), observer)
    } yield results + (name -> analysisState)
  }
}

enum ZeroAnalysisDiagnosticType:
  case Warning
  case Error

case class ZeroAnalysisInterpreterAbstractState(diagnostics: Set[(ZeroAnalysisDiagnosticType, String)] = Set()):
  def addDiagnostic(diagnosticType: ZeroAnalysisDiagnosticType, message: String): ZeroAnalysisInterpreterAbstractState = {
    ZeroAnalysisInterpreterAbstractState(diagnostics + ((diagnosticType, message)))
  }

class ZeroAnalysisInterpreterAnalysisState:
  var abstractStates: mutable.Map[ProgramPoint, ZeroAnalysisInterpreterAbstractState] = mutable.Map()

case class ZeroAnalysisInterpreter(cfg: Cfg, zeroAnalysisState: AnalysisState[ZeroAnalysisAbstractValue]) extends GraphAnalyser[ProgramPoint, ZeroAnalysisInterpreterAbstractState, ZeroAnalysisInterpreterAnalysisState]:
  override def entryNodes: Set[ProgramPoint] = cfg.entryPoints

  override def nextNodes(abstractState: ZeroAnalysisInterpreterAbstractState, node: ProgramPoint): Try[Set[ProgramPoint]] = Success(cfg.successors(node))

  override def initialiseAnalysisState(): Try[ZeroAnalysisInterpreterAnalysisState] = Success(ZeroAnalysisInterpreterAnalysisState())

  override def analyseNode(analysisState: ZeroAnalysisInterpreterAnalysisState, node: ProgramPoint): Try[ZeroAnalysisInterpreterAbstractState] = {
    val abstractState = analysisState.abstractStates.getOrElse(node, ZeroAnalysisInterpreterAbstractState())

    zeroAnalysisState.abstractStates.get(node) match {
      case Some(zeroAnalysisAbstractState) => node match {
        case ProgramPoint.StatementPoint(AssignStatement(lineNumber, _, ArithmeticBinaryOperation(_, ArithmeticBinaryOperator.Div, Variable(z)))) =>
          Try(zeroAnalysisAbstractState(z)) match {
            case Success(ZeroAnalysisAbstractValue.Zero) => Success(abstractState.addDiagnostic(ZeroAnalysisDiagnosticType.Error, s"Division by zero at line $lineNumber"))
            case Success(ZeroAnalysisAbstractValue.Unknown) => Success(abstractState.addDiagnostic(ZeroAnalysisDiagnosticType.Warning, s"Potential division by zero at line $lineNumber"))
            case _ => Success(abstractState)
          }
        case _ => Success(abstractState)
      }
      case None => Success(abstractState)
    }
  }

  override def updateAbstractState(analysisState: ZeroAnalysisInterpreterAnalysisState, from: ProgramPoint, to: ProgramPoint, abstractState: ZeroAnalysisInterpreterAbstractState): Try[Option[ZeroAnalysisInterpreterAbstractState]] =
    Success(Some(abstractState))

  override def getAbstractState(analysisState: ZeroAnalysisInterpreterAnalysisState, node: ProgramPoint): Try[Option[ZeroAnalysisInterpreterAbstractState]] = Success(analysisState.abstractStates.get(node))

  override def setAbstractState(analysisState: ZeroAnalysisInterpreterAnalysisState, node: ProgramPoint, abstractState: ZeroAnalysisInterpreterAbstractState): Try[Unit] = {
    analysisState.abstractStates.addOne(node -> abstractState)
    Success(())
  }

  override def merge(analysisState: ZeroAnalysisInterpreterAnalysisState, node: ProgramPoint, left: ZeroAnalysisInterpreterAbstractState, right: ZeroAnalysisInterpreterAbstractState): Try[ZeroAnalysisInterpreterAbstractState] = {
    Success(ZeroAnalysisInterpreterAbstractState(left.diagnostics ++ right.diagnostics))
  }

def zeroAnalysisInterpreter(cfgs: Map[String, Cfg], zeroAnalyses: Map[String, AnalysisState[ZeroAnalysisAbstractValue]], observer: AnalysisObserver[ProgramPoint, ZeroAnalysisInterpreterAbstractState, ZeroAnalysisInterpreterAnalysisState]): Try[Map[String, ZeroAnalysisInterpreterAbstractState]] = {
  cfgs.foldLeft(Try(Map.empty[String, ZeroAnalysisInterpreterAbstractState])) { (acc, entry) =>
    for {
      results <- acc
      (name, cfg) = entry
      zeroAnalysis <- zeroAnalyses.get(name) match {
        case Some(zeroAnalysis) => Success(zeroAnalysis)
        case None => Failure(new Exception(s"Zero analysis not found for $name"))
      }
      analysisState <- analysis[ProgramPoint, ZeroAnalysisInterpreterAbstractState, ZeroAnalysisInterpreterAnalysisState, ZeroAnalysisInterpreter, AnalysisObserver[ProgramPoint, ZeroAnalysisInterpreterAbstractState, ZeroAnalysisInterpreterAnalysisState]](ZeroAnalysisInterpreter(cfg, zeroAnalysis), observer)
    } yield results + (name -> analysisState.abstractStates(ProgramPoint.ExitPoint))
  }
}
