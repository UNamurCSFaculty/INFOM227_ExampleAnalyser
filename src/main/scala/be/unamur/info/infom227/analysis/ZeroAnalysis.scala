package be.unamur.info.infom227.analysis

import be.unamur.info.infom227.ast
import be.unamur.info.infom227.ast.*
import be.unamur.info.infom227.cfg.{Cfg, ProgramPoint}

import scala.collection.mutable
import scala.util.{Success, Try}

enum ZeroAnalysisAbstractValue extends Lattice[ZeroAnalysisAbstractValue]:
  case Top
  case Zero
  case NonZero
  case Bottom

  override def join(other: ZeroAnalysisAbstractValue): ZeroAnalysisAbstractValue = {
    (this, other) match {
      case (ZeroAnalysisAbstractValue.Bottom, _) => other
      case (_, ZeroAnalysisAbstractValue.Bottom) => this
      case (ZeroAnalysisAbstractValue.Zero, ZeroAnalysisAbstractValue.Zero) => ZeroAnalysisAbstractValue.Zero
      case (ZeroAnalysisAbstractValue.NonZero, ZeroAnalysisAbstractValue.NonZero) => ZeroAnalysisAbstractValue.NonZero
      case _ => ZeroAnalysisAbstractValue.Top
    }
  }

  override def meet(other: ZeroAnalysisAbstractValue): ZeroAnalysisAbstractValue = {
    (this, other) match {
      case (ZeroAnalysisAbstractValue.Top, _) => other
      case (_, ZeroAnalysisAbstractValue.Top) => this
      case (ZeroAnalysisAbstractValue.Zero, ZeroAnalysisAbstractValue.Zero) => ZeroAnalysisAbstractValue.Zero
      case (ZeroAnalysisAbstractValue.NonZero, ZeroAnalysisAbstractValue.NonZero) => ZeroAnalysisAbstractValue.NonZero
      case _ => ZeroAnalysisAbstractValue.Bottom
    }
  }

def abstractionFunction(value: Int | Boolean): ZeroAnalysisAbstractValue = {
  value match {
    case 0 => ZeroAnalysisAbstractValue.Zero
    case intValue: Int => ZeroAnalysisAbstractValue.NonZero
    case _ => ZeroAnalysisAbstractValue.Top
  }
}

case class ZeroAnalysisAbstractState(variables: Map[String, ZeroAnalysisAbstractValue] = Map()):
  override def toString: String = {
    val builder = new StringBuilder()
    for ((variable, abstractValue) <- variables) {
      builder.append(s"$variable: $abstractValue\n")
    }
    builder.toString()
  }

class ZeroAnalysisAnalysisState:
  var abstractStates: mutable.Map[ProgramPoint, ZeroAnalysisAbstractState] = mutable.Map()

case class ZeroAnalysis(cfg: Cfg) extends GraphAnalyser[ProgramPoint, ZeroAnalysisAbstractState, ZeroAnalysisAnalysisState]:
  def analyseExpression(abstractState: ZeroAnalysisAbstractState, expression: Expression | FunctionCall): Try[ZeroAnalysisAbstractValue] = {
    expression match {
      case arithmeticConstant: ArithmeticConstant => Success(abstractionFunction(arithmeticConstant.value))
      case arithmeticBinaryOperation: ArithmeticBinaryOperation =>
        for {
          leftAbstractValue <- analyseExpression(abstractState, arithmeticBinaryOperation.left)
          rightAbstractValue <- analyseExpression(abstractState, arithmeticBinaryOperation.right)
        } yield (leftAbstractValue, arithmeticBinaryOperation.operator, rightAbstractValue) match {
          case (ZeroAnalysisAbstractValue.Zero, ArithmeticBinaryOperator.Mul, _) => ZeroAnalysisAbstractValue.Zero
          case (_, ArithmeticBinaryOperator.Mul, ZeroAnalysisAbstractValue.Zero) => ZeroAnalysisAbstractValue.Zero
          case (ZeroAnalysisAbstractValue.Top, _, _) => ZeroAnalysisAbstractValue.Top
          case (_, _, ZeroAnalysisAbstractValue.Top) => ZeroAnalysisAbstractValue.Top
          // Add
          case (ZeroAnalysisAbstractValue.Zero, ArithmeticBinaryOperator.Add, ZeroAnalysisAbstractValue.Zero) => ZeroAnalysisAbstractValue.Zero
          case (ZeroAnalysisAbstractValue.Zero, ArithmeticBinaryOperator.Add, ZeroAnalysisAbstractValue.NonZero) => ZeroAnalysisAbstractValue.NonZero
          case (ZeroAnalysisAbstractValue.NonZero, ArithmeticBinaryOperator.Add, ZeroAnalysisAbstractValue.Zero) => ZeroAnalysisAbstractValue.NonZero
          case (ZeroAnalysisAbstractValue.NonZero, ArithmeticBinaryOperator.Add, ZeroAnalysisAbstractValue.NonZero) => ZeroAnalysisAbstractValue.NonZero
          // Sub
          case (ZeroAnalysisAbstractValue.Zero, ArithmeticBinaryOperator.Sub, ZeroAnalysisAbstractValue.Zero) => ZeroAnalysisAbstractValue.Zero
          case (ZeroAnalysisAbstractValue.Zero, ArithmeticBinaryOperator.Sub, ZeroAnalysisAbstractValue.NonZero) => ZeroAnalysisAbstractValue.NonZero
          case (ZeroAnalysisAbstractValue.NonZero, ArithmeticBinaryOperator.Sub, ZeroAnalysisAbstractValue.NonZero) => ZeroAnalysisAbstractValue.NonZero
          // Rest
          case _ => ZeroAnalysisAbstractValue.Top
        }
      case booleanConstant: BooleanConstant => Success(ZeroAnalysisAbstractValue.Top)
      case booleanNegOperation: BooleanNegOperation => Success(ZeroAnalysisAbstractValue.Top)
      case booleanBinaryOperation: BooleanBinaryOperation => Success(ZeroAnalysisAbstractValue.Top)
      case integerComparisonOperation: IntegerComparisonOperation => Success(ZeroAnalysisAbstractValue.Top)
      case variable: Variable => Success(abstractState.variables.getOrElse(variable.name, ZeroAnalysisAbstractValue.Bottom))
      case functionCall: FunctionCall => Success(ZeroAnalysisAbstractValue.Top)
    }
  }

  override def entryNodes: Set[ProgramPoint] = cfg.entryPoints

  override def nextNodes(abstractState: ZeroAnalysisAbstractState, node: ProgramPoint): Try[Set[ProgramPoint]] = Success(cfg.successors(node))

  override def initialiseAnalysisState(): Try[ZeroAnalysisAnalysisState] = Success(ZeroAnalysisAnalysisState())

  override def analyseNode(analysisState: ZeroAnalysisAnalysisState, node: ProgramPoint): Try[ZeroAnalysisAbstractState] = {
    val abstractState = analysisState.abstractStates.getOrElse(node, ZeroAnalysisAbstractState())

    node match {
      case ProgramPoint.StatementPoint(statement) =>
        statement match {
          case AssignStatement(lineNumber, variable, expression) => for {
            expressionAbstractValue <- analyseExpression(abstractState, expression)
          } yield {
            ZeroAnalysisAbstractState(abstractState.variables + (variable -> expressionAbstractValue))
          }
          case _ => Success(abstractState)
        }
      case _ => Success(abstractState)
    }
  }

  override def updateAbstractState(analysisState: ZeroAnalysisAnalysisState, from: ProgramPoint, to: ProgramPoint, abstractState: ZeroAnalysisAbstractState): Try[Option[ZeroAnalysisAbstractState]] =
    cfg.condition(from, to) match {
      case Some(IntegerComparisonOperation(left, EqualComparisonOperator.Eq, ArithmeticConstant(0))) =>
        for {
          leftAbstractValue <- analyseExpression(abstractState, left)
        } yield leftAbstractValue match {
          case ZeroAnalysisAbstractValue.NonZero => None
          case ZeroAnalysisAbstractValue.Zero => Some(abstractState)
          case _ => left match {
            case Variable(leftVariable) => Some(ZeroAnalysisAbstractState(abstractState.variables + (leftVariable -> ZeroAnalysisAbstractValue.Zero)))
            case _ => Some(abstractState)
          }
        }
      case Some(IntegerComparisonOperation(left, EqualComparisonOperator.Ne, ArithmeticConstant(0))) =>
        for {
          leftAbstractValue <- analyseExpression(abstractState, left)
        } yield leftAbstractValue match {
          case ZeroAnalysisAbstractValue.Zero => None
          case ZeroAnalysisAbstractValue.NonZero => Some(abstractState)
          case _ => left match {
            case Variable(leftVariable) => Some(ZeroAnalysisAbstractState(abstractState.variables + (leftVariable -> ZeroAnalysisAbstractValue.NonZero)))
            case _ => Some(abstractState)
          }
        }
      case _ => Success(Some(abstractState))
    }

  override def getAbstractState(analysisState: ZeroAnalysisAnalysisState, node: ProgramPoint): Try[Option[ZeroAnalysisAbstractState]] = Success(analysisState.abstractStates.get(node))

  override def setAbstractState(analysisState: ZeroAnalysisAnalysisState, node: ProgramPoint, abstractState: ZeroAnalysisAbstractState): Try[Unit] = {
    analysisState.abstractStates.addOne(node -> abstractState)
    Success(())
  }

  override def merge(analysisState: ZeroAnalysisAnalysisState, node: ProgramPoint, left: ZeroAnalysisAbstractState, right: ZeroAnalysisAbstractState): Try[ZeroAnalysisAbstractState] = {
    Success(ZeroAnalysisAbstractState(left.variables.foldLeft(right.variables) { (acc, entry) =>
      val (name, newAbstractValue) = entry
      val mergedValue = acc.get(name) match {
        case Some(currentAbstractValue) => newAbstractValue.join(currentAbstractValue)
        case None => newAbstractValue
      }
      acc + (name -> mergedValue)
    }))
  }

case class ZeroAnalysisObserver() extends AnalysisObserver[ProgramPoint, ZeroAnalysisAnalysisState]:
  override def afterNodeAnalysis(analysisState: ZeroAnalysisAnalysisState, worklist: mutable.Set[ProgramPoint], node: ProgramPoint): Unit = {
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

def analyseProgram(cfgs: Map[String, Cfg], observer: AnalysisObserver[ProgramPoint, ZeroAnalysisAnalysisState]): Try[Map[String, ZeroAnalysisAbstractState]] = {
  cfgs.foldLeft(Try(Map.empty[String, ZeroAnalysisAbstractState])) { (acc, entry) =>
    for {
      results <- acc
      (name, cfg) = entry
      analysisState <- analysis[ProgramPoint, ZeroAnalysisAbstractState, ZeroAnalysisAnalysisState, ZeroAnalysis, AnalysisObserver[ProgramPoint, ZeroAnalysisAnalysisState]](ZeroAnalysis(cfg), observer)
    } yield results + (name -> analysisState.abstractStates(ProgramPoint.ExitPoint))
  }
}
