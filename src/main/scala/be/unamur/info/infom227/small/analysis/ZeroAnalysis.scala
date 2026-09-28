package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast.{ArithmeticBinaryOperation, ArithmeticBinaryOperator, ArithmeticConstant, AssignStatement, BooleanConstant, BooleanExpression, BooleanNegOperation, EqualComparisonOperator, Expression, FunctionCall, IntegerComparisonOperation, IntegerComparisonOperator, Statement, Variable}
import be.unamur.info.infom227.small.cfg.{Cfg, ProgramPoint}

import scala.annotation.tailrec
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

class ZeroAnalysis(cfg: Cfg) extends ForwardMayAnalyzer[AbstractState[ZeroAnalysisAbstractValue]](cfg):
  override def entryAbstractState(): AbstractState[ZeroAnalysisAbstractValue] = AbstractState(cfg.parameters.map { parameter => parameter -> ZeroAnalysisAbstractValue.Unknown }.toMap)

  @tailrec
  final override def analyseStatement(abstractState: AbstractState[ZeroAnalysisAbstractValue], statement: Statement): AbstractState[ZeroAnalysisAbstractValue] = {
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
  final override def conditionUpdate(abstractState: AbstractState[ZeroAnalysisAbstractValue], condition: BooleanExpression): Option[AbstractState[ZeroAnalysisAbstractValue]] = {
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

def zeroAnalysis(cfgs: Map[String, Cfg], observer: AnalysisObserver[ProgramPoint, AbstractState[ZeroAnalysisAbstractValue], AnalysisState[AbstractState[ZeroAnalysisAbstractValue]]]): Try[Map[String, AnalysisState[AbstractState[ZeroAnalysisAbstractValue]]]] = {
  cfgs.foldLeft(Try(Map.empty[String, AnalysisState[AbstractState[ZeroAnalysisAbstractValue]]])) { (acc, entry) =>
    for {
      results <- acc
      (name, cfg) = entry
      analysisState <- analysis[ProgramPoint, AbstractState[ZeroAnalysisAbstractValue], AnalysisState[AbstractState[ZeroAnalysisAbstractValue]], ZeroAnalysis, AnalysisObserver[ProgramPoint, AbstractState[ZeroAnalysisAbstractValue], AnalysisState[AbstractState[ZeroAnalysisAbstractValue]]]](ZeroAnalysis(cfg), observer)
    } yield results + (name -> analysisState)
  }
}

enum ZeroAnalysisDiagnosticType:
  case Warning
  case Error

case class ZeroAnalysisInterpreterAbstractState(diagnostics: Set[(ZeroAnalysisDiagnosticType, String)] = Set()) extends Lattice[ZeroAnalysisInterpreterAbstractState]:
  def addDiagnostic(diagnosticType: ZeroAnalysisDiagnosticType, message: String): ZeroAnalysisInterpreterAbstractState = {
    ZeroAnalysisInterpreterAbstractState(diagnostics + ((diagnosticType, message)))
  }

  override def join(other: ZeroAnalysisInterpreterAbstractState): ZeroAnalysisInterpreterAbstractState = {
    ZeroAnalysisInterpreterAbstractState(diagnostics ++ other.diagnostics)
  }

  override def meet(other: ZeroAnalysisInterpreterAbstractState): ZeroAnalysisInterpreterAbstractState = {
    ZeroAnalysisInterpreterAbstractState(diagnostics.intersect(other.diagnostics))
  }

class ZeroAnalysisInterpreter(cfg: Cfg, zeroAnalysisState: AnalysisState[AbstractState[ZeroAnalysisAbstractValue]]) extends ForwardMayAnalyzer[ZeroAnalysisInterpreterAbstractState](cfg):
  override def entryAbstractState(): ZeroAnalysisInterpreterAbstractState = ZeroAnalysisInterpreterAbstractState()

  override def analyseStatement(abstractState: ZeroAnalysisInterpreterAbstractState, statement: Statement): ZeroAnalysisInterpreterAbstractState = {
    zeroAnalysisState.abstractStates.get(ProgramPoint.StatementPoint(statement)) match {
      case Some(zeroAnalysisAbstractState) => statement match {
        case AssignStatement(lineNumber, _, ArithmeticBinaryOperation(_, ArithmeticBinaryOperator.Div, Variable(z))) =>
          Try(zeroAnalysisAbstractState(z)) match {
            case Success(ZeroAnalysisAbstractValue.Zero) => abstractState.addDiagnostic(ZeroAnalysisDiagnosticType.Error, s"Division by zero at line $lineNumber")
            case Success(ZeroAnalysisAbstractValue.Unknown) => abstractState.addDiagnostic(ZeroAnalysisDiagnosticType.Warning, s"Potential division by zero at line $lineNumber")
            case _ => abstractState
          }
        case _ => abstractState
      }
      case None => abstractState
    }
  }

  override def conditionUpdate(abstractState: ZeroAnalysisInterpreterAbstractState, condition: BooleanExpression): Option[ZeroAnalysisInterpreterAbstractState] = {
    Some(abstractState)
  }


def zeroAnalysisInterpreter(cfgs: Map[String, Cfg], zeroAnalyses: Map[String, AnalysisState[AbstractState[ZeroAnalysisAbstractValue]]], observer: AnalysisObserver[ProgramPoint, ZeroAnalysisInterpreterAbstractState, AnalysisState[ZeroAnalysisInterpreterAbstractState]]): Try[Map[String, ZeroAnalysisInterpreterAbstractState]] = {
  cfgs.foldLeft(Try(Map.empty[String, ZeroAnalysisInterpreterAbstractState])) { (acc, entry) =>
    for {
      results <- acc
      (name, cfg) = entry
      zeroAnalysis <- zeroAnalyses.get(name) match {
        case Some(zeroAnalysis) => Success(zeroAnalysis)
        case None => Failure(new Exception(s"Zero analysis not found for $name"))
      }
      analysisState <- analysis[ProgramPoint, ZeroAnalysisInterpreterAbstractState, AnalysisState[ZeroAnalysisInterpreterAbstractState], ZeroAnalysisInterpreter, AnalysisObserver[ProgramPoint, ZeroAnalysisInterpreterAbstractState, AnalysisState[ZeroAnalysisInterpreterAbstractState]]](ZeroAnalysisInterpreter(cfg, zeroAnalysis), observer)
    } yield results + (name -> analysisState.abstractStates(ProgramPoint.ExitPoint))
  }
}
