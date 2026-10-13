package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast
import be.unamur.info.infom227.small.ast.*
import be.unamur.info.infom227.small.cfg.{Cfg, ProgramPoint}

import scala.util.{Success, Try}

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

type ConstantAnalysisAbstractState = AbstractState[ConstantAnalysisAbstractValue]

class ConstantAnalysis(cfg: Cfg) extends ForwardMayAnalyzer[ConstantAnalysisAbstractState](cfg):
  override def entryAbstractState(): ConstantAnalysisAbstractState = AbstractState(cfg.parameters.map { parameter => parameter -> ConstantAnalysisAbstractValue.Unknown }.toMap)

  final override def analyseStatement(
    abstractState: ConstantAnalysisAbstractState,
    statement: Statement
  ): ConstantAnalysisAbstractState = {
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
                case ArithmeticBinaryOperator.Div => if (d == 0) {
                  abstractState(variable -> ConstantAnalysisAbstractValue.Unknown)
                } else {
                  abstractState(variable -> ConstantAnalysisAbstractValue.Constant(c / d))
                }
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

  final override def conditionUpdate(
    abstractState: ConstantAnalysisAbstractState,
    condition: BooleanExpression
  ): Option[ConstantAnalysisAbstractState] = {
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

type ExtendedConstantAnalysisAbstractState = ExtendedAbstractState[ConstantAnalysisAbstractValue]
type ExtendedConstantAnalysisState = AnalysisState[ExtendedConstantAnalysisAbstractState]

class InterproceduralConstantAnalysis(
  name: String,
  cfgs: Map[String, Cfg],
  analysisState: InterproceduralConstantAnalysisState
) extends ForwardMayAnalyzer[ExtendedConstantAnalysisAbstractState](cfgs(name)):
  override def entryAbstractState(): ExtendedConstantAnalysisAbstractState = {
    val arguments = analysisState.functions.get(name).map(function => function.arguments).getOrElse(List[ConstantAnalysisAbstractValue]())
    ExtendedAbstractState(AbstractState(cfgs(name).parameters.zip(arguments).toMap))
  }

  private def analyseExpression(
    abstractState: ExtendedConstantAnalysisAbstractState,
    expression: Expression
  ): ConstantAnalysisAbstractValue = {
    expression match {
      case ArithmeticConstant(c) => ConstantAnalysisAbstractValue.Constant(c)
      case Variable(y) => abstractState(y)
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
            case ArithmeticBinaryOperator.Add => ConstantAnalysisAbstractValue.Constant(c + d)
            case ArithmeticBinaryOperator.Sub => ConstantAnalysisAbstractValue.Constant(c - d)
            case ArithmeticBinaryOperator.Mul => ConstantAnalysisAbstractValue.Constant(c * d)
            case ArithmeticBinaryOperator.Div => if (d == 0) {
              ConstantAnalysisAbstractValue.Unknown
            } else {
              ConstantAnalysisAbstractValue.Constant(c / d)
            }
          }
          case (_, _) => ConstantAnalysisAbstractValue.Unknown
        }
      case _ =>
        ConstantAnalysisAbstractValue.Unknown
    }
  }

  final override def analyseStatement(
    abstractState: ExtendedConstantAnalysisAbstractState,
    statement: Statement
  ): ExtendedConstantAnalysisAbstractState = {
    statement match {
      case AssignStatement(lineNumber, variable, expression: Expression) =>
        abstractState(variable -> analyseExpression(abstractState, expression))
      case AssignStatement(lineNumber, variable, expression: FunctionCall) =>
        abstractState.addCall(expression.name, expression.arguments.map(expression => analyseExpression(abstractState, expression)))(variable -> (analysisState.functions.get(expression.name) match {
          case Some(function) => function.returnValue.getOrElse(ConstantAnalysisAbstractValue.Bottom)
          case None => ConstantAnalysisAbstractValue.Bottom
        }))
      case ReturnStatement(lineNumber, expression) =>
        abstractState.withReturnValue(analyseExpression(abstractState, expression))
      case _ =>
        abstractState
    }
  }

  final override def conditionUpdate(
    abstractState: ExtendedConstantAnalysisAbstractState,
    condition: BooleanExpression
  ): Option[ExtendedConstantAnalysisAbstractState] = {
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

type InterproceduralConstantAnalysisObserver = AnalysisObserver[ProgramPoint, ExtendedConstantAnalysisAbstractState, ExtendedConstantAnalysisState]
type InterproceduralConstantAnalysisAbstractState[O] = InterproceduralAbstractState[ExtendedConstantAnalysisAbstractState, O]
type InterproceduralConstantAnalysisState = InterproceduralAnalysisState[ConstantAnalysisAbstractValue]

class InterproceduralConstantCallGraphAnalysis[O <: InterproceduralConstantAnalysisObserver](
  val cfgs: Map[String, Cfg],
  val observerFactory: (name: String, cfg: Cfg) => Try[O]
) extends CallGraphAnalyser[String, List[ConstantAnalysisAbstractValue], Option[ConstantAnalysisAbstractValue], InterproceduralConstantAnalysisAbstractState[O], InterproceduralConstantAnalysisState]:
  override def entryNodes: Set[String] = Set("main")

  override def calleeNodes(
    analysisState: InterproceduralConstantAnalysisState,
    node: String
  ): Try[Set[String]] = {
    Success(analysisState.functions(node).callees)
  }

  override def callerNodes(
    analysisState: InterproceduralConstantAnalysisState,
    node: String
  ): Try[Set[String]] = {
    Success(analysisState.functions.foldLeft(Set.empty[String]) { case (acc, (caller, functionState)) => if (functionState.callees.contains(node)) acc + caller else acc })
  }

  override def initialiseAnalysisState(): Try[InterproceduralConstantAnalysisState] = {
    Success(InterproceduralAnalysisState(
      cfgs.map((name, cfg) => name -> FunctionState(arguments = cfg.parameters.map(_ => ConstantAnalysisAbstractValue.Bottom)))
    ))
  }

  override def analyseNode(
    analysisState: InterproceduralConstantAnalysisState,
    node: String
  ): Try[InterproceduralConstantAnalysisAbstractState[O]] = {
    for {
      observer <- observerFactory(node, cfgs(node))
      constantAnalysisState <- analysis(InterproceduralConstantAnalysis(node, cfgs, analysisState), observer)
    } yield InterproceduralAbstractState(node, constantAnalysisState.abstractStates(ProgramPoint.ExitPoint), observer)
  }

  override def merge(
    analysisState: InterproceduralConstantAnalysisState,
    abstractState: InterproceduralConstantAnalysisAbstractState[O]
  ): Try[InterproceduralConstantAnalysisState] = {
    val constantAbstractState = abstractState.abstractState
    val previousFunctionState = analysisState.functions(abstractState.name)
    val functionState = previousFunctionState.join(FunctionState(
      constantAbstractState.state,
      previousFunctionState.arguments,
      constantAbstractState.returnValue,
      constantAbstractState.calls.keys.toSet
    ))
    val functions = analysisState.functions + (abstractState.name -> functionState)
    Success(InterproceduralAnalysisState(functions.map((name, functionState) => {
      constantAbstractState.calls.get(name) match {
        case Some(arguments) => name -> FunctionState(
          functionState.abstractState,
          joinList(functionState.arguments, arguments),
          functionState.returnValue,
          functionState.callees
        )
        case None => name -> functionState
      }
    })))
  }

  override def getArguments(
    analysisState: InterproceduralConstantAnalysisState,
    node: String
  ): Try[List[ConstantAnalysisAbstractValue]] = {
    Success(analysisState.functions(node).arguments)
  }

  override def getReturnValue(
    analysisState: InterproceduralConstantAnalysisState,
    node: String
  ): Try[Option[ConstantAnalysisAbstractValue]] = {
    Success(analysisState.functions(node).returnValue)
  }