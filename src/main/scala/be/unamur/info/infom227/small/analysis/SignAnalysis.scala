package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast.*
import be.unamur.info.infom227.small.cfg.Cfg

import scala.annotation.tailrec

enum SignAnalysisAbstractValue extends Lattice[SignAnalysisAbstractValue]:
  case Unknown
  case Negative
  case Zero
  case Positive
  case Bottom

  override def join(other: SignAnalysisAbstractValue): SignAnalysisAbstractValue = {
    (this, other) match {
      case (SignAnalysisAbstractValue.Bottom, _) => other
      case (_, SignAnalysisAbstractValue.Bottom) => this
      case (SignAnalysisAbstractValue.Zero, SignAnalysisAbstractValue.Zero) => SignAnalysisAbstractValue.Zero
      case (SignAnalysisAbstractValue.Positive, SignAnalysisAbstractValue.Positive) => SignAnalysisAbstractValue.Positive
      case (SignAnalysisAbstractValue.Negative, SignAnalysisAbstractValue.Negative) => SignAnalysisAbstractValue.Negative
      case _ => SignAnalysisAbstractValue.Unknown
    }
  }

  override def meet(other: SignAnalysisAbstractValue): SignAnalysisAbstractValue = {
    (this, other) match {
      case (SignAnalysisAbstractValue.Unknown, _) => other
      case (_, SignAnalysisAbstractValue.Unknown) => this
      case (SignAnalysisAbstractValue.Zero, SignAnalysisAbstractValue.Zero) => SignAnalysisAbstractValue.Zero
      case (SignAnalysisAbstractValue.Positive, SignAnalysisAbstractValue.Positive) => SignAnalysisAbstractValue.Positive
      case (SignAnalysisAbstractValue.Negative, SignAnalysisAbstractValue.Negative) => SignAnalysisAbstractValue.Negative
      case _ => SignAnalysisAbstractValue.Bottom
    }
  }

  override def toString: String = {
    this match {
      case SignAnalysisAbstractValue.Unknown => "U"
      case SignAnalysisAbstractValue.Negative => "LT"
      case SignAnalysisAbstractValue.Zero => "Z"
      case SignAnalysisAbstractValue.Positive => "GT"
      case SignAnalysisAbstractValue.Bottom => "⊥"
    }
  }

class SignAnalysis(cfg: Cfg) extends ForwardMayAnalyzer[AbstractState[SignAnalysisAbstractValue]](cfg):
  override def entryAbstractState(): AbstractState[SignAnalysisAbstractValue] = AbstractState(cfg.parameters.map { parameter => parameter -> SignAnalysisAbstractValue.Unknown }.toMap)

  final override def analyseStatement(abstractState: AbstractState[SignAnalysisAbstractValue], statement: Statement): AbstractState[SignAnalysisAbstractValue] = {
    statement match {
      case AssignStatement(lineNumber, variable, expression) =>
        expression match {
          case ArithmeticConstant(c) =>
            if (c == 0) {
              abstractState(variable -> SignAnalysisAbstractValue.Zero)
            } else if (c > 0) {
              abstractState(variable -> SignAnalysisAbstractValue.Positive)
            } else {
              abstractState(variable -> SignAnalysisAbstractValue.Negative)
            }
          case Variable(y) =>
            abstractState(variable -> abstractState(y))
          case ArithmeticBinaryOperation(ArithmeticConstant(0), ArithmeticBinaryOperator.Sub, ArithmeticConstant(d)) =>
            abstractState(variable -> SignAnalysisAbstractValue.Negative)
          case ArithmeticBinaryOperation(Variable(y), ArithmeticBinaryOperator.Mul, Variable(z)) =>
            if (abstractState(y) == SignAnalysisAbstractValue.Zero || abstractState(z) == SignAnalysisAbstractValue.Zero) {
              abstractState(variable -> SignAnalysisAbstractValue.Zero)
            } else if (abstractState(y) == SignAnalysisAbstractValue.Positive && abstractState(z) == SignAnalysisAbstractValue.Positive || abstractState(y) == SignAnalysisAbstractValue.Negative && abstractState(z) == SignAnalysisAbstractValue.Negative) {
              abstractState(variable -> SignAnalysisAbstractValue.Positive)
            } else if (abstractState(y) == SignAnalysisAbstractValue.Positive && abstractState(z) == SignAnalysisAbstractValue.Negative || abstractState(y) == SignAnalysisAbstractValue.Positive && abstractState(z) == SignAnalysisAbstractValue.Negative) {
              abstractState(variable -> SignAnalysisAbstractValue.Negative)
            } else {
              abstractState(variable -> SignAnalysisAbstractValue.Unknown)
            }
          case _ =>
            abstractState(variable -> SignAnalysisAbstractValue.Unknown)
        }
      case _ =>
        abstractState
    }
  }

  @tailrec
  final override def conditionUpdate(abstractState: AbstractState[SignAnalysisAbstractValue], condition: BooleanExpression): Option[AbstractState[SignAnalysisAbstractValue]] = {
    condition match {
      case BooleanConstant(true) =>
        Some(abstractState)
      case BooleanConstant(false) =>
        None
      case IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Lt, ArithmeticConstant(0)) =>
        Some(abstractState(y -> SignAnalysisAbstractValue.Negative))
      case IntegerComparisonOperation(ArithmeticConstant(c), IntegerComparisonOperator.Lt, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Gt, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Gt, ArithmeticConstant(c)) =>
        Some(abstractState(y -> SignAnalysisAbstractValue.Positive))
      case IntegerComparisonOperation(ArithmeticConstant(c), IntegerComparisonOperator.Gt, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), IntegerComparisonOperator.Lt, ArithmeticConstant(c)))
      case IntegerComparisonOperation(Variable(y), EqualComparisonOperator.Eq, ArithmeticConstant(0)) =>
        Some(abstractState(y -> SignAnalysisAbstractValue.Zero))
      case IntegerComparisonOperation(ArithmeticConstant(c), EqualComparisonOperator.Eq, Variable(y)) =>
        conditionUpdate(abstractState, IntegerComparisonOperation(Variable(y), EqualComparisonOperator.Eq, ArithmeticConstant(c)))
      case _ =>
        Some(abstractState)
    }
  }
