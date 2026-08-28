package be.unamur.info.infom227.small.interpreter

import be.unamur.info.infom227.small.ast.{ArithmeticBinaryOperation, ArithmeticBinaryOperator, ArithmeticConstant, ArithmeticExpression, AssignStatement, BooleanBinaryOperation, BooleanBinaryOperator, BooleanConstant, BooleanExpression, BooleanNegOperation, EqualComparisonOperator, Expression, FunctionCall, IfStatement, IntegerComparisonOperation, IntegerComparisonOperator, Program, ReturnStatement, Statement, Variable, WhileStatement}

import scala.collection.immutable.HashMap
import scala.util.{Failure, Success, Try}

type VariableType = Int | Boolean

def execute(program: Program, entry: String): Try[VariableType] = {
  program.functions.get(entry) match {
    case Some(entryFunction) =>
      if (entryFunction.parameters.nonEmpty) {
        Failure(RuntimeException("The entry function should not have parameters."))
      } else {
        for {
          (_, returnValue) <- executeFunction(program, Environments().push(), entryFunction.body)
        } yield returnValue
      }
    case None =>
      Failure(RuntimeException("No entry function found in the program."))
  }
}

def executeFunction(program: Program, environments: Environments[VariableType], body: List[Statement]): Try[(Environments[VariableType], VariableType)] = {
  for {
    (newEnvironments, returnValueOption) <- executeSequence(program, environments, body)
    updatedEnvironments <- newEnvironments.pop()
    returnValue <- returnValueOption match {
      case Some(value) => Success(value)
      case None => Failure(RuntimeException("Function did not return a value."))
    }
  } yield (updatedEnvironments, returnValue)
}

def executeSequence(program: Program, environments: Environments[VariableType], sequence: List[Statement]): Try[(Environments[VariableType], Option[VariableType])] = {
  sequence match {
    case Nil => Success((environments, None))
    case statement :: rest =>
      for {
        (newEnvironments, returnValueOption) <- executeStatement(program, environments, statement)
        result <- returnValueOption match {
          case Some(returnValue) => Success((newEnvironments, Some(returnValue)))
          case None => executeSequence(program, newEnvironments, rest)
        }
      } yield result
  }
}

def executeStatement(program: Program, environments: Environments[VariableType], statement: Statement): Try[(Environments[VariableType], Option[VariableType])] = {
  statement match {
    case assignStatement: AssignStatement => executeAssignStatement(program, environments, assignStatement)
    case ifStatement: IfStatement => executeIfStatement(program, environments, ifStatement)
    case whileStatement: WhileStatement => executeWhileStatement(program, environments, whileStatement)
    case returnStatement: ReturnStatement => executeReturnStatement(program, environments, returnStatement)
  }
}

def executeAssignStatement(program: Program, environments: Environments[VariableType], assignStatement: AssignStatement): Try[(Environments[VariableType], Option[VariableType])] = {
  assignStatement.expression match {
    case expression: Expression => for {
      value <- executeExpression(environments, expression)
      newEnvironments <- environments.updated(assignStatement.variable, value)
    } yield (newEnvironments, None)
    case call: FunctionCall =>
      for {
        (newEnvironments, returnValue) <- executeCall(program, environments, call)
        updatedEnvironments <- environments.updated(assignStatement.variable, returnValue)
      } yield (updatedEnvironments, None)
  }
}

def executeIfStatement(program: Program, environments: Environments[VariableType], ifStatement: IfStatement): Try[(Environments[VariableType], Option[VariableType])] = {
  for {
    conditionValue <- executeBooleanExpression(environments, ifStatement.condition)
    result <- if (conditionValue) {
      executeSequence(program, environments, ifStatement.ifBody)
    } else {
      executeSequence(program, environments, ifStatement.elseBody)
    }
  } yield result
}

def executeWhileStatement(program: Program, environments: Environments[VariableType], whileStatement: WhileStatement): Try[(Environments[VariableType], Option[VariableType])] = {
  for {
    conditionValue <- executeBooleanExpression(environments, whileStatement.condition)
    result <- if (conditionValue) {
      for {
        (newEnvironments, returnValueOption) <- executeSequence(program, environments, whileStatement.body)
        finalResult <- returnValueOption match {
          case Some(returnValue) => Success((newEnvironments, Some(returnValue)))
          case None => executeWhileStatement(program, newEnvironments, whileStatement)
        }
      } yield finalResult
    } else {
      Success((environments, None))
    }
  } yield result
}

def executeReturnStatement(program: Program, environments: Environments[VariableType], returnStatement: ReturnStatement): Try[(Environments[VariableType], Option[VariableType])] = {
  for {
    returnValue <- executeExpression(environments, returnStatement.expression)
  } yield (environments, Some(returnValue))
}

def executeExpression(environments: Environments[VariableType], expression: Expression): Try[VariableType] = {
  expression match {
    case variable: Variable => environments.get(variable.name)
    case arithmeticExpression: ArithmeticExpression => executeArithmeticExpression(environments, arithmeticExpression)
    case booleanExpression: BooleanExpression => executeBooleanExpression(environments, booleanExpression)
  }
}

def executeArithmeticExpression(environments: Environments[VariableType], arithmeticExpression: ArithmeticExpression): Try[Int] = {
  arithmeticExpression match {
    case ArithmeticConstant(value) => Success(value)
    case ArithmeticBinaryOperation(left, operator, right) =>
      for {
        leftValue <- executeArithmeticExpression(environments, left)
        rightValue <- executeArithmeticExpression(environments, right)
        result <- operator match {
          case ArithmeticBinaryOperator.Add => Success(leftValue + rightValue)
          case ArithmeticBinaryOperator.Sub => Success(leftValue - rightValue)
          case ArithmeticBinaryOperator.Mul => Success(leftValue * rightValue)
          case ArithmeticBinaryOperator.Div =>
            if (rightValue == 0) {
              Failure(RuntimeException("Division by zero."))
            } else {
              Success(leftValue / rightValue)
            }
        }
      } yield result
    case Variable(name) =>
      for {
        value <- environments.get(name)
        intValue <- value match {
          case intValue: Int => Success(intValue)
          case _ => Failure(RuntimeException(s"Variable '$name' is not an integer."))
        }
      } yield intValue
  }
}

def executeBooleanExpression(environments: Environments[VariableType], booleanExpression: BooleanExpression): Try[Boolean] = {
  booleanExpression match {
    case BooleanConstant(value) => Success(value)
    case BooleanNegOperation(value) =>
      for {
        boolValue <- executeBooleanExpression(environments, value)
      } yield !boolValue
    case BooleanBinaryOperation(left, operator, right) =>
      for {
        leftValue <- executeBooleanExpression(environments, left)
        rightValue <- executeBooleanExpression(environments, right)
        result <- operator match {
          case BooleanBinaryOperator.And => Success(leftValue && rightValue)
          case BooleanBinaryOperator.Or => Success(leftValue || rightValue)
          case EqualComparisonOperator.Eq => Success(leftValue == rightValue)
          case EqualComparisonOperator.Ne => Success(leftValue != rightValue)
        }
      } yield result
    case IntegerComparisonOperation(left, operator, right) =>
      for {
        leftValue <- executeArithmeticExpression(environments, left)
        rightValue <- executeArithmeticExpression(environments, right)
        result <- operator match {
          case IntegerComparisonOperator.Gt => Success(leftValue > rightValue)
          case IntegerComparisonOperator.Lt => Success(leftValue < rightValue)
          case EqualComparisonOperator.Eq => Success(leftValue == rightValue)
          case EqualComparisonOperator.Ne => Success(leftValue != rightValue)
          case IntegerComparisonOperator.Gte => Success(leftValue >= rightValue)
          case IntegerComparisonOperator.Lte => Success(leftValue <= rightValue)
        }
      } yield result
    case Variable(name) =>
      for {
        value <- environments.get(name)
        boolValue <- value match {
          case boolValue: Boolean => Success(boolValue)
          case _ => Failure(RuntimeException(s"Variable '$name' is not a boolean."))
        }
      } yield boolValue
  }
}

def executeCall(program: Program, environments: Environments[VariableType], call: FunctionCall): Try[(Environments[VariableType], VariableType)] = {
  program.functions.get(call.name) match {
    case Some(function) =>
      for {
        arguments <- call.arguments.foldLeft(Success(List.empty[VariableType]): Try[List[VariableType]]) { (acc, arg) =>
          for {
            args <- acc
            value <- executeExpression(environments, arg)
          } yield args :+ value
        }
        _ <- if (function.parameters.length != arguments.length) {
          Failure(RuntimeException(s"Function '${call.name}' expects ${function.parameters.length} arguments, but got ${arguments.length}."))
        } else {
          Success(())
        }
        functionEnvironment = environments.push(HashMap(function.parameters.zip(arguments) *))
        result <- executeFunction(program, functionEnvironment, function.body)
      } yield result
    case None =>
      Failure(RuntimeException(s"Function '${call.name}' not found."))
  }
}
