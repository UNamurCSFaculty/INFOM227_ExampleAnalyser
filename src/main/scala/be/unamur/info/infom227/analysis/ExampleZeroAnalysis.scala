package be.unamur.info.infom227.analysis

import be.unamur.info.infom227.ast.*
import be.unamur.info.infom227.cfg.ExampleProgramPoint

import scala.util.{Failure, Success, Try}

enum ExampleZeroAnalysisAbstractValue:
  case Bottom, Z, NZ, U

object ExampleZeroAnalysisAbstractValue {
  val lattice: ExampleLattice[ExampleZeroAnalysisAbstractValue] = {
    ExampleFiniteSizeLattice(
      Set(
        (ExampleZeroAnalysisAbstractValue.Bottom, ExampleZeroAnalysisAbstractValue.Z),
        (ExampleZeroAnalysisAbstractValue.Bottom, ExampleZeroAnalysisAbstractValue.NZ),
        (ExampleZeroAnalysisAbstractValue.Z, ExampleZeroAnalysisAbstractValue.U),
        (ExampleZeroAnalysisAbstractValue.NZ, ExampleZeroAnalysisAbstractValue.U),
      )
    )
  }
}

private class ExampleZeroAnalysisFlowFunctions extends
  ExampleStatementVisitor[Try[ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]], ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]],
  ExampleExpressionVisitor[Try[ExampleZeroAnalysisAbstractValue], ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]] {

  override def visitExampleDeclareStatement(node: ExampleDeclareStatement, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]] = {
    node.exampleType match
      case ExampleInt => Success(abstractEnvironment.set(node.name, ExampleZeroAnalysisAbstractValue.Z))
      case ExampleBool => Success(abstractEnvironment.set(node.name, ExampleZeroAnalysisAbstractValue.NZ))
  }

  override def visitExampleAssignStatement(node: ExampleAssignStatement, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]] = {
    if (node.scope.statements.nonEmpty) {
      Success(abstractEnvironment.set(node.variable, ExampleZeroAnalysisAbstractValue.U))
    } else {
      node.expression.accept(this, abstractEnvironment).map(abstractEnvironment.set(node.variable, _))
    }
  }

  override def visitExamplePrintStatement(node: ExamplePrintStatement, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]] = {
    Success(abstractEnvironment)
  }

  override def visitExampleIfStatement(node: ExampleIfStatement, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]] = {
    Success(abstractEnvironment)
  }

  override def visitExampleWhileStatement(node: ExampleWhileStatement, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]] = {
    Success(abstractEnvironment)
  }

  override def visitExampleIntegerConstant(node: ExampleIntegerConstant, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleZeroAnalysisAbstractValue] = {
    if (node.value == 0) {
      Success(ExampleZeroAnalysisAbstractValue.Z)
    } else {
      Success(ExampleZeroAnalysisAbstractValue.NZ)
    }
  }

  override def visitExampleIntegerVariable(node: ExampleIntegerVariable, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleZeroAnalysisAbstractValue] = {
    Try(abstractEnvironment.abstractValues(node.name))
  }

  override def visitExampleIntegerUnaryOperation(node: ExampleIntegerUnaryOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleZeroAnalysisAbstractValue] = {
    Success(ExampleZeroAnalysisAbstractValue.U)
  }

  override def visitExampleIntegerBinaryOperation(node: ExampleIntegerBinaryOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleZeroAnalysisAbstractValue] = {
    node match
      case ExampleIntegerBinaryOperation(ExampleIntegerBinaryOperator.Add, ExampleIntegerConstant(c), ExampleIntegerConstant(d)) =>
        if (c == -d) {
          Success(ExampleZeroAnalysisAbstractValue.Z)
        } else {
          Success(ExampleZeroAnalysisAbstractValue.U)
        }
      case ExampleIntegerBinaryOperation(ExampleIntegerBinaryOperator.Add, ExampleIntegerVariable(y), ExampleIntegerVariable(z)) =>
        Try {
          (abstractEnvironment.abstractValues(y), abstractEnvironment.abstractValues(z)) match
            case (ExampleZeroAnalysisAbstractValue.Z, ExampleZeroAnalysisAbstractValue.Z) => ExampleZeroAnalysisAbstractValue.Z
            case (_, _) => ExampleZeroAnalysisAbstractValue.U
        }
      case ExampleIntegerBinaryOperation(ExampleIntegerBinaryOperator.Add, ExampleIntegerVariable(y), ExampleIntegerConstant(c)) =>
        Try {
          abstractEnvironment.abstractValues(y) match
            case ExampleZeroAnalysisAbstractValue.Z if c == 0 => ExampleZeroAnalysisAbstractValue.Z
            case ExampleZeroAnalysisAbstractValue.Z if c != 0 => ExampleZeroAnalysisAbstractValue.NZ
            case ExampleZeroAnalysisAbstractValue.NZ if c == 0 => ExampleZeroAnalysisAbstractValue.NZ
            case _ => ExampleZeroAnalysisAbstractValue.U
        }
      case ExampleIntegerBinaryOperation(ExampleIntegerBinaryOperator.Add, ExampleIntegerConstant(c), ExampleIntegerVariable(y)) =>
        visitExampleIntegerBinaryOperation(ExampleIntegerBinaryOperation(ExampleIntegerBinaryOperator.Add, ExampleIntegerVariable(y), ExampleIntegerConstant(c)), abstractEnvironment)
      case _ => Success(ExampleZeroAnalysisAbstractValue.U)
  }

  override def visitExampleBooleanConstant(node: ExampleBooleanConstant, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleZeroAnalysisAbstractValue] = {
    Success(ExampleZeroAnalysisAbstractValue.NZ)
  }

  override def visitExampleBooleanVariable(node: ExampleBooleanVariable, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleZeroAnalysisAbstractValue] = {
    Try(abstractEnvironment.abstractValues(node.name))
  }

  override def visitExampleBooleanUnaryOperation(node: ExampleBooleanUnaryOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleZeroAnalysisAbstractValue] = {
    Success(ExampleZeroAnalysisAbstractValue.U)
  }

  override def visitExampleBooleanBinaryOperation(node: ExampleBooleanBinaryOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleZeroAnalysisAbstractValue] = {
    Success(ExampleZeroAnalysisAbstractValue.U)
  }

  override def visitExampleBooleanEqualComparisonOperation(node: ExampleBooleanEqualComparisonOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleZeroAnalysisAbstractValue] = {
    Success(ExampleZeroAnalysisAbstractValue.U)
  }

  override def visitExampleIntegerComparisonOperation(node: ExampleIntegerComparisonOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleZeroAnalysisAbstractValue] = {
    Success(ExampleZeroAnalysisAbstractValue.U)
  }

  override def visitExampleIntegerEqualComparisonOperation(node: ExampleIntegerEqualComparisonOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleZeroAnalysisAbstractValue] = {
    Success(ExampleZeroAnalysisAbstractValue.U)
  }
}

private class ConditionUpdate() extends ExampleExpressionVisitor[Try[Map[String, ExampleZeroAnalysisAbstractValue]], ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]] {

  override def visitExampleIntegerConstant(node: ExampleIntegerConstant, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    Failure(NotImplementedError())
  }

  override def visitExampleIntegerVariable(node: ExampleIntegerVariable, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    Failure(NotImplementedError())
  }

  override def visitExampleIntegerUnaryOperation(node: ExampleIntegerUnaryOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    Failure(NotImplementedError())
  }

  override def visitExampleIntegerBinaryOperation(node: ExampleIntegerBinaryOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    Failure(NotImplementedError())
  }

  override def visitExampleBooleanConstant(node: ExampleBooleanConstant, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    Success(Map.empty)
  }

  override def visitExampleBooleanVariable(node: ExampleBooleanVariable, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    Success(Map.empty)
  }

  override def visitExampleBooleanUnaryOperation(node: ExampleBooleanUnaryOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    Success(Map.empty)
  }

  override def visitExampleBooleanBinaryOperation(node: ExampleBooleanBinaryOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    Success(Map.empty)
  }

  override def visitExampleBooleanEqualComparisonOperation(node: ExampleBooleanEqualComparisonOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    Success(Map.empty)
  }

  override def visitExampleIntegerComparisonOperation(node: ExampleIntegerComparisonOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    node match
      case ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Lt, ExampleIntegerVariable(y), ExampleIntegerConstant(c)) =>
        if (c <= 0) {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.NZ))
        } else {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.U))
        }
      case ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Lt, ExampleIntegerConstant(c), ExampleIntegerVariable(y)) =>
        visitExampleIntegerComparisonOperation(ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Gt, ExampleIntegerVariable(y), ExampleIntegerConstant(c)), abstractEnvironment)
      case ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Gt, ExampleIntegerVariable(y), ExampleIntegerConstant(c)) =>
        if (c >= 0) {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.NZ))
        } else {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.U))
        }
      case ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Gt, ExampleIntegerConstant(c), ExampleIntegerVariable(y)) =>
        visitExampleIntegerComparisonOperation(ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Lt, ExampleIntegerVariable(y), ExampleIntegerConstant(c)), abstractEnvironment)
      case ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Lte, ExampleIntegerVariable(y), ExampleIntegerConstant(c)) =>
        if (c < 0) {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.NZ))
        } else {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.U))
        }
      case ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Lte, ExampleIntegerConstant(c), ExampleIntegerVariable(y)) =>
        visitExampleIntegerComparisonOperation(ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Gte, ExampleIntegerVariable(y), ExampleIntegerConstant(c)), abstractEnvironment)
      case ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Gte, ExampleIntegerVariable(y), ExampleIntegerConstant(c)) =>
        if (c > 0) {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.NZ))
        } else {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.U))
        }
      case ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Gte, ExampleIntegerConstant(c), ExampleIntegerVariable(y)) =>
        visitExampleIntegerComparisonOperation(ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Lte, ExampleIntegerVariable(y), ExampleIntegerConstant(c)), abstractEnvironment)
      case _ => Success(Map.empty)
  }

  override def visitExampleIntegerEqualComparisonOperation(node: ExampleIntegerEqualComparisonOperation, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    node match
      case ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Eq, ExampleIntegerVariable(y), ExampleIntegerConstant(c)) =>
        if (c == 0) {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.Z))
        } else {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.NZ))
        }
      case ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Eq, ExampleIntegerConstant(c), ExampleIntegerVariable(y)) =>
        visitExampleIntegerEqualComparisonOperation(ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Eq, ExampleIntegerVariable(y), ExampleIntegerConstant(c)), abstractEnvironment)
      case ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Ne, ExampleIntegerVariable(y), ExampleIntegerConstant(c)) =>
        if (c == 0) {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.NZ))
        } else {
          Success(Map(y -> ExampleZeroAnalysisAbstractValue.U))
        }
      case ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Ne, ExampleIntegerConstant(c), ExampleIntegerVariable(y)) =>
        visitExampleIntegerEqualComparisonOperation(ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Ne, ExampleIntegerVariable(y), ExampleIntegerConstant(c)), abstractEnvironment)
      case _ => Success(Map.empty)
  }
}

object ExampleZeroAnalysisWorklist extends ExampleWorklist(ExampleZeroAnalysisAbstractValue.lattice) {
  override def controlFlowFunctions(p: ExampleProgramPoint, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]] = {
    p.statement.accept(ExampleZeroAnalysisFlowFunctions(), abstractEnvironment)
  }

  override def conditionUpdate(condition: ExampleBooleanExpression, abstractEnvironment: ExampleAbstractEnvironment[String, ExampleZeroAnalysisAbstractValue]): Try[Map[String, ExampleZeroAnalysisAbstractValue]] = {
    condition.accept(ConditionUpdate(), abstractEnvironment)
  }
}

object ExampleZeroAnalysisResultsInterpreter extends ExampleResultsInterpreter[ExampleZeroAnalysisAbstractValue](
  (p, results) => {
    p.statement match
      case ExampleAssignStatement(_, _, _, ExampleIntegerBinaryOperation(ExampleIntegerBinaryOperator.Div, ExampleIntegerVariable(_), ExampleIntegerVariable(z))) =>
        results
          .get(p)
          .flatMap(_.abstractValues.get(z))
          .flatMap {
            case ExampleZeroAnalysisAbstractValue.Z => Some(ExampleErrorMessageType.Error, "Division by zero detected !")
            case ExampleZeroAnalysisAbstractValue.U => Some(ExampleErrorMessageType.Warning, "Possible division by zero !")
            case _ => None
          }
      case _ => None
  }
)
