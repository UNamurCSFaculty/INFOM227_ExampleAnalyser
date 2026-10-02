package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast
import be.unamur.info.infom227.small.ast.*
import be.unamur.info.infom227.small.cfg.{Cfg, ProgramPoint}

import scala.util.{Success, Try}

enum IntervalNumber:
  case NegInf
  case Integer(value: Int)
  case PosInf

  def min(other: IntervalNumber): IntervalNumber = {
    (this, other) match {
      case (IntervalNumber.NegInf, _) => IntervalNumber.NegInf
      case (_, IntervalNumber.NegInf) => IntervalNumber.NegInf
      case (IntervalNumber.Integer(left), IntervalNumber.Integer(right)) => IntervalNumber.Integer(left min right)
      case (IntervalNumber.PosInf, right) => right
      case (left, IntervalNumber.PosInf) => left
    }
  }

  def max(other: IntervalNumber): IntervalNumber = {
    (this, other) match {
      case (IntervalNumber.PosInf, _) => IntervalNumber.PosInf
      case (_, IntervalNumber.PosInf) => IntervalNumber.PosInf
      case (IntervalNumber.Integer(left), IntervalNumber.Integer(right)) => IntervalNumber.Integer(left max right)
      case (IntervalNumber.NegInf, right) => right
      case (left, IntervalNumber.NegInf) => left
    }
  }

  override def toString: String = {
    this match {
      case IntervalNumber.NegInf => "-∞"
      case IntervalNumber.Integer(value) => value.toString
      case IntervalNumber.PosInf => "+∞"
    }
  }

enum IntervalAnalysisAbstractValue extends Lattice[IntervalAnalysisAbstractValue]:
  case Unknown
  case Interval(from: IntervalNumber, to: IntervalNumber)
  case Bottom

  override def join(other: IntervalAnalysisAbstractValue): IntervalAnalysisAbstractValue = {
    (this, other) match {
      case (IntervalAnalysisAbstractValue.Bottom, _) => other
      case (_, IntervalAnalysisAbstractValue.Bottom) => this
      case (IntervalAnalysisAbstractValue.Interval(leftFrom: IntervalNumber, leftTo: IntervalNumber), IntervalAnalysisAbstractValue.Interval(rightFrom: IntervalNumber, rightTo: IntervalNumber)) => IntervalAnalysisAbstractValue.interval(leftFrom.min(rightFrom), leftTo.max(rightTo))
      case _ => IntervalAnalysisAbstractValue.Unknown
    }
  }

  override def meet(other: IntervalAnalysisAbstractValue): IntervalAnalysisAbstractValue = {
    (this, other) match {
      case (IntervalAnalysisAbstractValue.Unknown, _) => other
      case (_, IntervalAnalysisAbstractValue.Unknown) => this
      case (IntervalAnalysisAbstractValue.Interval(leftFrom: IntervalNumber, leftTo: IntervalNumber), IntervalAnalysisAbstractValue.Interval(rightFrom: IntervalNumber, rightTo: IntervalNumber)) => IntervalAnalysisAbstractValue.interval(leftFrom.max(rightFrom), leftTo.min(rightTo))
      case _ => IntervalAnalysisAbstractValue.Bottom
    }
  }

  override def toString: String = {
    this match {
      case IntervalAnalysisAbstractValue.Unknown => "U"
      case IntervalAnalysisAbstractValue.Interval(from: IntervalNumber, to: IntervalNumber) => s"[$from, $to]"
      case IntervalAnalysisAbstractValue.Bottom => "⊥"
    }
  }

object IntervalAnalysisAbstractValue:
  def constant(number: IntervalNumber): IntervalAnalysisAbstractValue = {
    IntervalAnalysisAbstractValue.Interval(number, number)
  }

  def interval(from: IntervalNumber, to: IntervalNumber): IntervalAnalysisAbstractValue = {
    (from, to) match {
      case (IntervalNumber.Integer(left), IntervalNumber.Integer(right)) => if (left <= right) {
        IntervalAnalysisAbstractValue.Interval(IntervalNumber.Integer(left), IntervalNumber.Integer(right))
      } else {
        IntervalAnalysisAbstractValue.Bottom
      }
      case (IntervalNumber.PosInf, IntervalNumber.NegInf) => IntervalAnalysisAbstractValue.Bottom
      case (left, right) => IntervalAnalysisAbstractValue.Interval(left, right)
    }
  }

class IntervalAnalysis(cfg: Cfg, maxIteration: Int = 5) extends ForwardMayAnalyzer[IterationAbstractState[IntervalAnalysisAbstractValue]](cfg):
  override def entryAbstractState(): IterationAbstractState[IntervalAnalysisAbstractValue] = IterationAbstractState(state = AbstractState(cfg.parameters.map { parameter => parameter -> IntervalAnalysisAbstractValue.Unknown }.toMap))

  def evaluateOperation(left: IntervalNumber, operator: ArithmeticBinaryOperator, right: IntervalNumber): Option[IntervalNumber] = {
    (left, operator, right) match {
      case (IntervalNumber.Integer(leftVal), ArithmeticBinaryOperator.Add, IntervalNumber.Integer(rightVal)) => Some(IntervalNumber.Integer(leftVal + rightVal))
      case (IntervalNumber.Integer(leftVal), ArithmeticBinaryOperator.Sub, IntervalNumber.Integer(rightVal)) => Some(IntervalNumber.Integer(leftVal - rightVal))
      case (IntervalNumber.Integer(leftVal), ArithmeticBinaryOperator.Mul, IntervalNumber.Integer(rightVal)) => Some(IntervalNumber.Integer(leftVal * rightVal))
      case (IntervalNumber.Integer(leftVal), ArithmeticBinaryOperator.Div, IntervalNumber.Integer(rightVal)) => if (rightVal == 0) {
        None
      } else {
        Some(IntervalNumber.Integer(leftVal / rightVal))
      }
      case (IntervalNumber.PosInf, ArithmeticBinaryOperator.Add | ArithmeticBinaryOperator.Sub, IntervalNumber.Integer(_)) => Some(IntervalNumber.PosInf)
      case (IntervalNumber.PosInf, ArithmeticBinaryOperator.Mul | ArithmeticBinaryOperator.Div, IntervalNumber.Integer(c)) => if (c > 0) {
        Some(IntervalNumber.PosInf)
      } else if (c < 0) {
        Some(IntervalNumber.NegInf)
      } else {
        None
      }
      case (IntervalNumber.Integer(_), ArithmeticBinaryOperator.Add, IntervalNumber.PosInf) => Some(IntervalNumber.PosInf)
      case (IntervalNumber.Integer(_), ArithmeticBinaryOperator.Sub, IntervalNumber.PosInf) => Some(IntervalNumber.NegInf)
      case (IntervalNumber.Integer(c), ArithmeticBinaryOperator.Mul, IntervalNumber.PosInf) => if (c > 0) {
        Some(IntervalNumber.PosInf)
      } else if (c < 0) {
        Some(IntervalNumber.NegInf)
      } else {
        None
      }
      case (IntervalNumber.Integer(c), ArithmeticBinaryOperator.Div, IntervalNumber.PosInf) => Some(IntervalNumber.Integer(0))
      case (IntervalNumber.PosInf, ArithmeticBinaryOperator.Add, IntervalNumber.PosInf) => Some(IntervalNumber.PosInf)
      case (IntervalNumber.PosInf, ArithmeticBinaryOperator.Sub, IntervalNumber.NegInf) => Some(IntervalNumber.PosInf)
      case (IntervalNumber.NegInf, ArithmeticBinaryOperator.Add | ArithmeticBinaryOperator.Sub, IntervalNumber.Integer(_)) => Some(IntervalNumber.NegInf)
      case (IntervalNumber.NegInf, ArithmeticBinaryOperator.Mul | ArithmeticBinaryOperator.Div, IntervalNumber.Integer(c)) => if (c > 0) {
        Some(IntervalNumber.NegInf)
      } else if (c < 0) {
        Some(IntervalNumber.PosInf)
      } else {
        None
      }
      case (IntervalNumber.Integer(_), ArithmeticBinaryOperator.Add, IntervalNumber.NegInf) => Some(IntervalNumber.NegInf)
      case (IntervalNumber.Integer(_), ArithmeticBinaryOperator.Sub, IntervalNumber.NegInf) => Some(IntervalNumber.PosInf)
      case (IntervalNumber.Integer(c), ArithmeticBinaryOperator.Mul, IntervalNumber.NegInf) => if (c > 0) {
        Some(IntervalNumber.NegInf)
      } else if (c < 0) {
        Some(IntervalNumber.PosInf)
      } else {
        None
      }
      case (IntervalNumber.Integer(c), ArithmeticBinaryOperator.Div, IntervalNumber.NegInf) => Some(IntervalNumber.Integer(0))
      case (IntervalNumber.NegInf, ArithmeticBinaryOperator.Add, IntervalNumber.NegInf) => Some(IntervalNumber.NegInf)
      case (IntervalNumber.NegInf, ArithmeticBinaryOperator.Sub, IntervalNumber.PosInf) => Some(IntervalNumber.NegInf)
      case _ => None
    }
  }

  final override def analyseStatement(abstractState: IterationAbstractState[IntervalAnalysisAbstractValue], statement: Statement): IterationAbstractState[IntervalAnalysisAbstractValue] = {
    statement match {
      case AssignStatement(lineNumber, variable, expression) =>
        expression match {
          case ArithmeticConstant(c) => abstractState(variable -> IntervalAnalysisAbstractValue.Interval(IntervalNumber.Integer(c), IntervalNumber.Integer(c)))
          case Variable(y) => abstractState(variable -> abstractState(y))
          case ArithmeticBinaryOperation(left: (Variable | ArithmeticConstant), operator, right: (Variable | ArithmeticConstant)) =>
            val leftAbstractValue = left match {
              case Variable(y) => abstractState(y)
              case ArithmeticConstant(c) => IntervalAnalysisAbstractValue.constant(IntervalNumber.Integer(c))
            }
            val rightAbstractValue = right match {
              case Variable(z) => abstractState(z)
              case ArithmeticConstant(c) => IntervalAnalysisAbstractValue.constant(IntervalNumber.Integer(c))
            }
            (leftAbstractValue, rightAbstractValue) match {
              case (IntervalAnalysisAbstractValue.Interval(leftFrom, leftTo), IntervalAnalysisAbstractValue.Interval(rightFrom, rightTo)) =>
                (evaluateOperation(leftFrom, operator, rightFrom), evaluateOperation(leftTo, operator, rightTo)) match {
                  case (Some(newFrom), Some(newTo)) => abstractState(variable -> IntervalAnalysisAbstractValue.Interval(newFrom, newTo))
                  case (_, _) => abstractState(variable -> IntervalAnalysisAbstractValue.Unknown)
                }
              case (_, _) => abstractState(variable -> IntervalAnalysisAbstractValue.Unknown)
            }
          case _ =>
            abstractState(variable -> IntervalAnalysisAbstractValue.Unknown)
        }
      case _ =>
        abstractState
    }
  }

  final override def conditionUpdate(abstractState: IterationAbstractState[IntervalAnalysisAbstractValue], condition: BooleanExpression): Option[IterationAbstractState[IntervalAnalysisAbstractValue]] = {
    condition match {
      case BooleanConstant(true) =>
        Some(abstractState)
      case BooleanConstant(false) =>
        None
      case IntegerComparisonOperation(Variable(x), EqualComparisonOperator.Eq, ArithmeticConstant(c)) =>
        Some(abstractState(x -> IntervalAnalysisAbstractValue.constant(IntervalNumber.Integer(c))))
      case IntegerComparisonOperation(Variable(x), operator: IntegerComparisonOperator, ArithmeticConstant(c)) =>
        val validRange = operator match {
          case IntegerComparisonOperator.Lt => IntervalAnalysisAbstractValue.Interval(IntervalNumber.NegInf, IntervalNumber.Integer(c - 1))
          case IntegerComparisonOperator.Lte => IntervalAnalysisAbstractValue.Interval(IntervalNumber.NegInf, IntervalNumber.Integer(c))
          case IntegerComparisonOperator.Gt => IntervalAnalysisAbstractValue.Interval(IntervalNumber.Integer(c + 1), IntervalNumber.PosInf)
          case IntegerComparisonOperator.Gte => IntervalAnalysisAbstractValue.Interval(IntervalNumber.Integer(c), IntervalNumber.PosInf)
        }
        abstractState(x).meet(validRange) match {
          case IntervalAnalysisAbstractValue.Bottom => None
          case value => Some(abstractState(x -> value))
        }
      case IntegerComparisonOperation(ArithmeticConstant(c), operator: IntegerComparisonOperator, Variable(x)) =>
        val validRange = operator match {
          case IntegerComparisonOperator.Lt => IntervalAnalysisAbstractValue.Interval(IntervalNumber.Integer(c + 1), IntervalNumber.PosInf)
          case IntegerComparisonOperator.Lte => IntervalAnalysisAbstractValue.Interval(IntervalNumber.Integer(c), IntervalNumber.PosInf)
          case IntegerComparisonOperator.Gt => IntervalAnalysisAbstractValue.Interval(IntervalNumber.NegInf, IntervalNumber.Integer(c - 1))
          case IntegerComparisonOperator.Gte => IntervalAnalysisAbstractValue.Interval(IntervalNumber.NegInf, IntervalNumber.Integer(c))
        }
        abstractState(x).meet(validRange) match {
          case IntervalAnalysisAbstractValue.Bottom => None
          case value => Some(abstractState(x -> value))
        }
      case _ =>
        Some(abstractState)
    }
  }

  override def merge(analysisState: AnalysisState[IterationAbstractState[IntervalAnalysisAbstractValue]], node: ProgramPoint, left: IterationAbstractState[IntervalAnalysisAbstractValue], right: IterationAbstractState[IntervalAnalysisAbstractValue]): Try[IterationAbstractState[IntervalAnalysisAbstractValue]] = {
    var tryWiden = false

    val joined = left.joinWith(
      right,
      (leftValue, rightValue, iterations) => {
        (leftValue, rightValue) match {
          case (
            IntervalAnalysisAbstractValue.Interval(IntervalNumber.Integer(a), IntervalNumber.Integer(b)),
            IntervalAnalysisAbstractValue.Interval(IntervalNumber.Integer(c), IntervalNumber.Integer(d))
          ) =>
            if (iterations.getOrElse(node, 0) > maxIteration) {
              IntervalAnalysisAbstractValue.interval(
                if (a <= c) IntervalNumber.Integer(a) else IntervalNumber.NegInf,
                if (b >= d) IntervalNumber.Integer(b) else IntervalNumber.PosInf
              )
            } else {
              tryWiden = true
              leftValue.join(rightValue)
            }
          case (_, _) => leftValue.join(rightValue)
        }
      }
    )

    Success(
      if (tryWiden) {
        joined.increased(node)
      } else {
        joined
      }
    )
  }
