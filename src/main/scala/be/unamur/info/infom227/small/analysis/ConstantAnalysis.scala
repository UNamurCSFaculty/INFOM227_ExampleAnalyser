package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast
import be.unamur.info.infom227.small.ast.*
import be.unamur.info.infom227.small.cfg.Cfg

enum ConstantAnalysisAbstractValue extends Lattice[ConstantAnalysisAbstractValue]:
  case Unknown
  case Constant(value: Int | Boolean)
  case Bottom

  override def join(other: ConstantAnalysisAbstractValue): ConstantAnalysisAbstractValue = {
    (this, other) match {
      case (ConstantAnalysisAbstractValue.Bottom, _) => other
      case (_, ConstantAnalysisAbstractValue.Bottom) => this
      case (ConstantAnalysisAbstractValue.Constant(c), ConstantAnalysisAbstractValue.Constant(d)) if c == d => ConstantAnalysisAbstractValue.Constant(c)
      case _ => ConstantAnalysisAbstractValue.Unknown
    }
  }

  override def meet(other: ConstantAnalysisAbstractValue): ConstantAnalysisAbstractValue = {
    (this, other) match {
      case (ConstantAnalysisAbstractValue.Unknown, _) => other
      case (_, ConstantAnalysisAbstractValue.Unknown) => this
      case (ConstantAnalysisAbstractValue.Constant(c), ConstantAnalysisAbstractValue.Constant(d)) if c == d => ConstantAnalysisAbstractValue.Constant(c)
      case _ => ConstantAnalysisAbstractValue.Bottom
    }
  }

  override def toString: String = {
    this match {
      case ConstantAnalysisAbstractValue.Unknown => "U"
      case ConstantAnalysisAbstractValue.Constant(c) => c.toString
      case ConstantAnalysisAbstractValue.Bottom => "⊥"
    }
  }

class ConstantAnalysis(cfg: Cfg) extends ForwardMayAnalyzer[AbstractState[ConstantAnalysisAbstractValue]](cfg):
  override def entryAbstractState(): AbstractState[ConstantAnalysisAbstractValue] = AbstractState(cfg.parameters.map { parameter => parameter -> ConstantAnalysisAbstractValue.Unknown }.toMap)

  final override def analyseStatement(abstractState: AbstractState[ConstantAnalysisAbstractValue], statement: Statement): AbstractState[ConstantAnalysisAbstractValue] = {
    statement match {
      case AssignStatement(lineNumber, variable, expression) =>
        expression match {
          case ArithmeticConstant(c) => abstractState(variable -> ConstantAnalysisAbstractValue.Constant(c))
          case Variable(y) => abstractState(variable -> abstractState(y))
          case ArithmeticBinaryOperation(left: (Variable | ArithmeticConstant), operator, right: (Variable | ArithmeticConstant)) =>
            val leftAbstractValue = left match {
              case Variable(y) => abstractState(y)
              case ArithmeticConstant(c) => ConstantAnalysisAbstractValue.Constant(c)
            }
            val rightAbstractValue = right match {
              case Variable(z) => abstractState(z)
              case ArithmeticConstant(c) => ConstantAnalysisAbstractValue.Constant(c)
            }
            (leftAbstractValue, rightAbstractValue) match {
              case (ConstantAnalysisAbstractValue.Constant(c: Int), ConstantAnalysisAbstractValue.Constant(d: Int)) => operator match {
                case ArithmeticBinaryOperator.Add => abstractState(variable -> ConstantAnalysisAbstractValue.Constant(c + d))
                case ArithmeticBinaryOperator.Sub => abstractState(variable -> ConstantAnalysisAbstractValue.Constant(c - d))
                case ArithmeticBinaryOperator.Mul => abstractState(variable -> ConstantAnalysisAbstractValue.Constant(c * d))
                case ArithmeticBinaryOperator.Div => abstractState(variable -> ConstantAnalysisAbstractValue.Constant(c / d))
              }
              case (_, _) => abstractState(variable -> ConstantAnalysisAbstractValue.Unknown)
            }
          case _ =>
            abstractState(variable -> ConstantAnalysisAbstractValue.Unknown)
        }
      case _ =>
        abstractState
    }
  }

  final override def conditionUpdate(abstractState: AbstractState[ConstantAnalysisAbstractValue], condition: BooleanExpression): Option[AbstractState[ConstantAnalysisAbstractValue]] = {
    condition match {
      case BooleanConstant(true) =>
        Some(abstractState)
      case BooleanConstant(false) =>
        None
      case IntegerComparisonOperation(Variable(x), EqualComparisonOperator.Eq, ArithmeticConstant(c)) =>
        Some(abstractState(x -> ConstantAnalysisAbstractValue.Constant(c)))
      case IntegerComparisonOperation(ArithmeticConstant(c), EqualComparisonOperator.Eq, Variable(x)) =>
        Some(abstractState(x -> ConstantAnalysisAbstractValue.Constant(c)))
      case IntegerComparisonOperation(left: (Variable | ArithmeticConstant), operator, right: (Variable | ArithmeticConstant)) =>
        val leftAbstractValue = left match {
          case Variable(y) => abstractState(y)
          case ArithmeticConstant(c) => ConstantAnalysisAbstractValue.Constant(c)
        }
        val rightAbstractValue = right match {
          case Variable(z) => abstractState(z)
          case ArithmeticConstant(c) => ConstantAnalysisAbstractValue.Constant(c)
        }
        (leftAbstractValue, rightAbstractValue) match {
          case (ConstantAnalysisAbstractValue.Constant(c: Int), ConstantAnalysisAbstractValue.Constant(d: Int)) => operator match {
            case IntegerComparisonOperator.Lt => Some(abstractState).filter(_ => c < d)
            case IntegerComparisonOperator.Gt => Some(abstractState).filter(_ => c > d)
            case IntegerComparisonOperator.Lte => Some(abstractState).filter(_ => c <= d)
            case IntegerComparisonOperator.Gte => Some(abstractState).filter(_ => c >= d)
            case EqualComparisonOperator.Eq => Some(abstractState).filter(_ => c == d)
            case EqualComparisonOperator.Ne => Some(abstractState).filter(_ => c != d)
          }
          case (_, _) => Some(abstractState)
        }
      case _ =>
        Some(abstractState)
    }
  }
