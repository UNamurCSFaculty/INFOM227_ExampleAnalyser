package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast.{ArithmeticBinaryOperation, ArithmeticBinaryOperator, ArithmeticConstant, AssignStatement, BooleanBinaryOperation, BooleanBinaryOperator, BooleanConstant, BooleanExpression, BooleanNegOperation, EqualComparisonOperator, Expression, FunctionCall, IntegerComparisonOperation, IntegerComparisonOperator, Statement, Variable}
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

case class ZeroAnalysisAbstractState(variables: Map[String, ZeroAnalysisAbstractValue] = Map()):
  def get(variable: String): Try[ZeroAnalysisAbstractValue] = {
    variables.get(variable) match {
      case Some(value) => Success(value)
      case None => Failure(new RuntimeException(s"Variable $variable not found"))
    }
  }

  def update(variable: String, abstractValue: ZeroAnalysisAbstractValue): ZeroAnalysisAbstractState = {
    ZeroAnalysisAbstractState(variables + (variable -> abstractValue))
  }

  override def toString: String = {
    val builder = new StringBuilder()
    for ((variable, abstractValue) <- variables) {
      builder.append(s"$variable: $abstractValue\n")
    }
    builder.toString()
  }

class ZeroAnalysisState:
  var abstractStates: mutable.Map[ProgramPoint, ZeroAnalysisAbstractState] = mutable.Map()

case class ZeroAnalysis(cfg: Cfg) extends GraphAnalyser[ProgramPoint, ZeroAnalysisAbstractState, ZeroAnalysisState]:
  @tailrec
  private def analyseStatement(abstractState: ZeroAnalysisAbstractState, statement: Statement): Try[ZeroAnalysisAbstractState] = {
    statement match {
      case AssignStatement(lineNumber, variable, expression) =>
        expression match {
          case ArithmeticConstant(c) =>
            Success(if (c == 0) {
              abstractState.update(variable, ZeroAnalysisAbstractValue.Zero)
            } else {
              abstractState.update(variable, ZeroAnalysisAbstractValue.NonZero)
            })
          case Variable(y) =>
            for {
              yAbstractValue <- abstractState.get(y)
            } yield abstractState.update(variable, yAbstractValue)
          case ArithmeticBinaryOperation(ArithmeticConstant(c), ArithmeticBinaryOperator.Add, ArithmeticConstant(d)) =>
            Success(if (c == -d) {
              abstractState.update(variable, ZeroAnalysisAbstractValue.Zero)
            } else {
              abstractState.update(variable, ZeroAnalysisAbstractValue.Unknown)
            })
          case ArithmeticBinaryOperation(Variable(y), ArithmeticBinaryOperator.Add, Variable(z)) =>
            for {
              yAbstractValue <- abstractState.get(y)
              zAbstractValue <- abstractState.get(z)
            } yield if (yAbstractValue == ZeroAnalysisAbstractValue.Zero && zAbstractValue == ZeroAnalysisAbstractValue.Zero) {
              abstractState.update(variable, ZeroAnalysisAbstractValue.Zero)
            } else {
              abstractState.update(variable, ZeroAnalysisAbstractValue.Unknown)
            }
          case ArithmeticBinaryOperation(Variable(y), ArithmeticBinaryOperator.Add, ArithmeticConstant(c)) =>
            for {
              yAbstractValue <- abstractState.get(y)
            } yield if (yAbstractValue == ZeroAnalysisAbstractValue.Zero && c == 0) {
              abstractState.update(variable, ZeroAnalysisAbstractValue.Zero)
            } else if (yAbstractValue == ZeroAnalysisAbstractValue.Zero && c != 0) {
              abstractState.update(variable, ZeroAnalysisAbstractValue.NonZero)
            } else if (yAbstractValue == ZeroAnalysisAbstractValue.NonZero && c == 0) {
              abstractState.update(variable, ZeroAnalysisAbstractValue.NonZero)
            } else {
              abstractState.update(variable, ZeroAnalysisAbstractValue.Unknown)
            }
          case ArithmeticBinaryOperation(ArithmeticConstant(c), ArithmeticBinaryOperator.Add, Variable(y)) =>
            analyseStatement(abstractState, AssignStatement(lineNumber, variable, ArithmeticBinaryOperation(Variable(y), ArithmeticBinaryOperator.Add, ArithmeticConstant(c))))
          case _ =>
            Success(abstractState.update(variable, ZeroAnalysisAbstractValue.Unknown))
        }
      case _ =>
        Success(abstractState)
    }
  }

  @tailrec
  private def conditionUpdate(abstractState: ZeroAnalysisAbstractState, condition: BooleanExpression): Try[Option[ZeroAnalysisAbstractState]] = {
    condition match {
      case BooleanConstant(true) =>
        Success(Some(abstractState))
      case BooleanConstant(false) =>
        Success(None)
      case IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Lt, ArithmeticConstant(c)) =>
        for {
          yAbstractValue <- abstractState.get(y)
          metAbstractValue = yAbstractValue.meet(ZeroAnalysisAbstractValue.NonZero)
        } yield if (c <= 0 && metAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState.update(y, metAbstractValue))
        } else if (c > 0) {
          Some(abstractState)
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), IntegerComparisonOperator.Lt, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Gt, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Gt, ArithmeticConstant(c)) =>
        for {
          yAbstractValue <- abstractState.get(y)
          metAbstractValue = yAbstractValue.meet(ZeroAnalysisAbstractValue.NonZero)
        } yield if (c >= 0 && metAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState.update(y, metAbstractValue))
        } else if (c < 0) {
          Some(abstractState)
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), IntegerComparisonOperator.Gt, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Lt, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Lte, ArithmeticConstant(c)) =>
        for {
          yAbstractValue <- abstractState.get(y)
          metAbstractValue = yAbstractValue.meet(ZeroAnalysisAbstractValue.NonZero)
        } yield if (c < 0 && metAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState.update(y, metAbstractValue))
        } else if (c >= 0) {
          Some(abstractState)
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), IntegerComparisonOperator.Lte, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Gte, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Gte, ArithmeticConstant(c)) =>
        for {
          yAbstractValue <- abstractState.get(y)
          metAbstractValue = yAbstractValue.meet(ZeroAnalysisAbstractValue.NonZero)
        } yield if (c > 0 && metAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState.update(y, metAbstractValue))
        } else if (c <= 0) {
          Some(abstractState)
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), IntegerComparisonOperator.Gte, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Lte, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), EqualComparisonOperator.Eq, ArithmeticConstant(c)) =>
        for {
          yAbstractValue <- abstractState.get(y)
          metZeroAbstractValue = yAbstractValue.meet(ZeroAnalysisAbstractValue.Zero)
          metNonZeroAbstractValue = yAbstractValue.meet(ZeroAnalysisAbstractValue.NonZero)
        } yield if (c == 0 && metZeroAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState.update(y, metZeroAbstractValue))
        } else if (c != 0 && metNonZeroAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState.update(y, metNonZeroAbstractValue))
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), EqualComparisonOperator.Eq, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), EqualComparisonOperator.Eq, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), EqualComparisonOperator.Ne, ArithmeticConstant(c)) =>
        for {
          yAbstractValue <- abstractState.get(y)
          metNonZeroAbstractValue = yAbstractValue.meet(ZeroAnalysisAbstractValue.NonZero)
        } yield if (c == 0 && metNonZeroAbstractValue != ZeroAnalysisAbstractValue.Bottom) {
          Some(abstractState.update(y, metNonZeroAbstractValue))
        } else if (c != 0) {
          Some(abstractState)
        } else {
          None
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), EqualComparisonOperator.Ne, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), EqualComparisonOperator.Ne, ArithmeticConstant(c)))
      case _ =>
        Success(Some(abstractState))
    }
  }

  override def entryNodes: Set[ProgramPoint] = cfg.entryPoints

  override def nextNodes(abstractState: ZeroAnalysisAbstractState, node: ProgramPoint): Try[Set[ProgramPoint]] = Success(cfg.successors(node))

  override def initialiseAnalysisState(): Try[ZeroAnalysisState] = Success(ZeroAnalysisState())

  override def analyseNode(analysisState: ZeroAnalysisState, node: ProgramPoint): Try[ZeroAnalysisAbstractState] = {
    val abstractState = analysisState.abstractStates.getOrElse(node, ZeroAnalysisAbstractState())

    node match {
      case ProgramPoint.StatementPoint(statement) => analyseStatement(abstractState, statement)
      case _ => Success(abstractState)
    }
  }

  override def updateAbstractState(analysisState: ZeroAnalysisState, from: ProgramPoint, to: ProgramPoint, abstractState: ZeroAnalysisAbstractState): Try[Option[ZeroAnalysisAbstractState]] =
    cfg.condition(from, to) match {
      case Some(condition) => conditionUpdate(abstractState, condition)
      case _ => Failure(new RuntimeException("condition should always exist"))
    }

  override def getAbstractState(analysisState: ZeroAnalysisState, node: ProgramPoint): Try[Option[ZeroAnalysisAbstractState]] = Success(analysisState.abstractStates.get(node))

  override def setAbstractState(analysisState: ZeroAnalysisState, node: ProgramPoint, abstractState: ZeroAnalysisAbstractState): Try[Unit] = {
    analysisState.abstractStates.addOne(node -> abstractState)
    Success(())
  }

  override def merge(analysisState: ZeroAnalysisState, node: ProgramPoint, left: ZeroAnalysisAbstractState, right: ZeroAnalysisAbstractState): Try[ZeroAnalysisAbstractState] = {
    Success(ZeroAnalysisAbstractState(left.variables.foldLeft(right.variables) { (acc, entry) =>
      val (name, newAbstractValue) = entry
      val mergedValue = acc.get(name) match {
        case Some(currentAbstractValue) => newAbstractValue.join(currentAbstractValue)
        case None => newAbstractValue
      }
      acc + (name -> mergedValue)
    }))
  }

case class ZeroAnalysisObserver() extends AnalysisObserver[ProgramPoint, ZeroAnalysisState]:
  override def afterNodeAnalysis(analysisState: ZeroAnalysisState, worklist: mutable.Set[ProgramPoint], node: ProgramPoint): Unit = {
    println(s"Program point $node:")
    val abstractStateString = analysisState.abstractStates.get(node) match {
      case Some(abstractState) => abstractState.toString()
      case None => ""
    }
    if (abstractStateString.nonEmpty) {
      for (line <- abstractStateString.split("\n")) {
        println(s"  $line")
      }
    } else {
      println("  /")
    }
  }

def zeroAnalysis(cfgs: Map[String, Cfg], observer: AnalysisObserver[ProgramPoint, ZeroAnalysisState]): Try[Map[String, ZeroAnalysisState]] = {
  cfgs.foldLeft(Try(Map.empty[String, ZeroAnalysisState])) { (acc, entry) =>
    for {
      results <- acc
      (name, cfg) = entry
      analysisState <- analysis[ProgramPoint, ZeroAnalysisAbstractState, ZeroAnalysisState, ZeroAnalysis, AnalysisObserver[ProgramPoint, ZeroAnalysisState]](ZeroAnalysis(cfg), observer)
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

case class ZeroAnalysisInterpreter(cfg: Cfg, zeroAnalysisState: ZeroAnalysisState) extends GraphAnalyser[ProgramPoint, ZeroAnalysisInterpreterAbstractState, ZeroAnalysisInterpreterAnalysisState]:
  override def entryNodes: Set[ProgramPoint] = cfg.entryPoints

  override def nextNodes(abstractState: ZeroAnalysisInterpreterAbstractState, node: ProgramPoint): Try[Set[ProgramPoint]] = Success(cfg.successors(node))

  override def initialiseAnalysisState(): Try[ZeroAnalysisInterpreterAnalysisState] = Success(ZeroAnalysisInterpreterAnalysisState())

  override def analyseNode(analysisState: ZeroAnalysisInterpreterAnalysisState, node: ProgramPoint): Try[ZeroAnalysisInterpreterAbstractState] = {
    val abstractState = analysisState.abstractStates.getOrElse(node, ZeroAnalysisInterpreterAbstractState())

    zeroAnalysisState.abstractStates.get(node) match {
      case Some(zeroAnalysisAbstractState) => node match {
        case ProgramPoint.StatementPoint(AssignStatement(lineNumber, _, ArithmeticBinaryOperation(_, ArithmeticBinaryOperator.Div, Variable(z)))) =>
          zeroAnalysisAbstractState.get(z) match {
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

def zeroAnalysisInterpreter(cfgs: Map[String, Cfg], zeroAnalyses: Map[String, ZeroAnalysisState], observer: AnalysisObserver[ProgramPoint, ZeroAnalysisInterpreterAnalysisState]): Try[Map[String, ZeroAnalysisInterpreterAbstractState]] = {
  cfgs.foldLeft(Try(Map.empty[String, ZeroAnalysisInterpreterAbstractState])) { (acc, entry) =>
    for {
      results <- acc
      (name, cfg) = entry
      zeroAnalysis <- zeroAnalyses.get(name) match {
        case Some(zeroAnalysis) => Success(zeroAnalysis)
        case None => Failure(new Exception(s"Zero analysis not found for $name"))
      }
      analysisState <- analysis[ProgramPoint, ZeroAnalysisInterpreterAbstractState, ZeroAnalysisInterpreterAnalysisState, ZeroAnalysisInterpreter, AnalysisObserver[ProgramPoint, ZeroAnalysisInterpreterAnalysisState]](ZeroAnalysisInterpreter(cfg, zeroAnalysis), observer)
    } yield results + (name -> analysisState.abstractStates(ProgramPoint.ExitPoint))
  }
}
