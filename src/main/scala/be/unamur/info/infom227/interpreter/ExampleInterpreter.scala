package be.unamur.info.infom227.interpreter

import be.unamur.info.infom227.ast.*
import be.unamur.info.infom227.util.TryIterator.*

import scala.util.{Success, Try}

object ExampleInterpreter {
  def run(programAst: ExampleProgram, environment: ExampleEnvironment[Int | Boolean] = ExampleEnvironment()): Try[Unit] = {
    ExampleInterpreter().visitExampleProgram(programAst, environment)
  }
}

private class ExampleInterpreter extends ExampleAstVisitor[Try[Int | Boolean | Unit], ExampleEnvironment[Int | Boolean]] {
  override def visitExampleProgram(node: ExampleProgram, environment: ExampleEnvironment[Int | Boolean]): Try[Unit] = {
    visitExampleScope(node.scope, environment)
  }

  override def visitExampleScope(node: ExampleScope, environment: ExampleEnvironment[Int | Boolean]): Try[Unit] = {
    node.statements.iterator.map(_.accept(this, environment)).toTry.map(_ => ())
  }

  override def visitExampleStatements(node: ExampleStatements, environment: ExampleEnvironment[Int | Boolean]): Try[Unit] = {
    node.statements.iterator.map(_.accept(this, environment)).toTry.map(_ => ())
  }

  override def visitExampleDeclareStatement(node: ExampleDeclareStatement, environment: ExampleEnvironment[Int | Boolean]): Try[Unit] = {
    environment.define(node.name, node.exampleType match
      case ExampleInt => 0
      case ExampleBool => false
    )
  }

  override def visitExampleAssignStatement(node: ExampleAssignStatement, environment: ExampleEnvironment[Int | Boolean]): Try[Unit] = {
    val scopeEnvironment = if (node.scope.statements.isEmpty) environment else ExampleEnvironment(environment)

    for {
      _ <- visitExampleScope(node.scope, scopeEnvironment)
      case value: (Int | Boolean) <- node.expression.accept(this, scopeEnvironment)
      _ <- environment.set(node.variable, value)
    } yield ()
  }

  override def visitExamplePrintStatement(node: ExamplePrintStatement, environment: ExampleEnvironment[Int | Boolean]): Try[Unit] = {
    for {
      case value: (Int | Boolean) <- node.expression.accept(this, environment)
      _ <- environment.print(
        value match
          case intValue: Int => intValue.toString
          case boolValue: Boolean => if (boolValue) "True" else "False"
      )
    } yield ()
  }

  override def visitExampleIfStatement(node: ExampleIfStatement, environment: ExampleEnvironment[Int | Boolean]): Try[Unit] = {
    for {
      case condition: Boolean <- node.condition.accept(this, environment)
      _ <- if (condition) {
        node.ifStatements.accept(this, environment)
      } else {
        node.elseStatements.accept(this, environment)
      }
    } yield ()
  }

  override def visitExampleWhileStatement(node: ExampleWhileStatement, environment: ExampleEnvironment[Int | Boolean]): Try[Unit] = {
    for {
      case condition: Boolean <- node.condition.accept(this, environment)
      _ <- if (condition) {
        for {
          _ <- node.statements.accept(this, environment)
          _ <- visitExampleWhileStatement(node, environment)
        } yield ()
      } else {
        Success(())
      }
    } yield ()
  }

  override def visitExampleInt(node: ExampleInt.type, environment: ExampleEnvironment[Int | Boolean]): Try[Unit] = Success(())

  override def visitExampleBool(node: ExampleBool.type, environment: ExampleEnvironment[Int | Boolean]): Try[Unit] = Success(())

  override def visitExampleIntegerConstant(node: ExampleIntegerConstant, environment: ExampleEnvironment[Int | Boolean]): Try[Int] = {
    Success(node.value)
  }

  override def visitExampleIntegerVariable(node: ExampleIntegerVariable, environment: ExampleEnvironment[Int | Boolean]): Try[Int] = {
    for {
      case value: Int <- environment.get(node.name)
    } yield value
  }

  override def visitExampleIntegerUnaryOperation(node: ExampleIntegerUnaryOperation, environment: ExampleEnvironment[Int | Boolean]): Try[Int] = {
    for {
      case value: Int <- node.value.accept(this, environment)
    } yield node.operator match
      case ExampleIntegerUnaryOperator.Pos => value
      case ExampleIntegerUnaryOperator.Neg => -value
  }

  override def visitExampleIntegerBinaryOperation(node: ExampleIntegerBinaryOperation, environment: ExampleEnvironment[Int | Boolean]): Try[Int] = {
    for {
      case leftValue: Int <- node.left.accept(this, environment)
      case rightValue: Int <- node.right.accept(this, environment)
    } yield node.operator match
      case ExampleIntegerBinaryOperator.Add => leftValue + rightValue
      case ExampleIntegerBinaryOperator.Sub => leftValue - rightValue
      case ExampleIntegerBinaryOperator.Mul => leftValue * rightValue
      case ExampleIntegerBinaryOperator.Div => leftValue / rightValue
  }

  override def visitExampleBooleanConstant(node: ExampleBooleanConstant, environment: ExampleEnvironment[Int | Boolean]): Try[Boolean] = {
    Success(node.value)
  }

  override def visitExampleBooleanVariable(node: ExampleBooleanVariable, environment: ExampleEnvironment[Int | Boolean]): Try[Boolean] = {
    for {
      case value: Boolean <- environment.get(node.name)
    } yield value
  }

  override def visitExampleBooleanUnaryOperation(node: ExampleBooleanUnaryOperation, environment: ExampleEnvironment[Int | Boolean]): Try[Boolean] = {
    for {
      case value: Boolean <- node.value.accept(this, environment)
    } yield node.operator match
      case ExampleBooleanUnaryOperator.Neg => !value
  }

  override def visitExampleBooleanBinaryOperation(node: ExampleBooleanBinaryOperation, environment: ExampleEnvironment[Int | Boolean]): Try[Boolean] = {
    for {
      case leftValue: Boolean <- node.left.accept(this, environment)
      case rightValue: Boolean <- node.right.accept(this, environment)
    } yield node.operator match
      case ExampleBooleanBinaryOperator.And => leftValue && rightValue
      case ExampleBooleanBinaryOperator.Or => leftValue || rightValue
  }

  override def visitExampleBooleanEqualComparisonOperation(node: ExampleBooleanEqualComparisonOperation, environment: ExampleEnvironment[Int | Boolean]): Try[Boolean] = {
    for {
      case leftValue: Boolean <- node.left.accept(this, environment)
      case rightValue: Boolean <- node.right.accept(this, environment)
    } yield node.operator match
      case ExampleEqualComparisonOperator.Eq => leftValue == rightValue
      case ExampleEqualComparisonOperator.Ne => leftValue != rightValue
  }

  override def visitExampleIntegerComparisonOperation(node: ExampleIntegerComparisonOperation, environment: ExampleEnvironment[Int | Boolean]): Try[Boolean] = {
    for {
      case leftValue: Int <- node.left.accept(this, environment)
      case rightValue: Int <- node.right.accept(this, environment)
    } yield node.operator match
      case ExampleIntegerComparisonOperator.Lt => leftValue < rightValue
      case ExampleIntegerComparisonOperator.Gt => leftValue > rightValue
      case ExampleIntegerComparisonOperator.Lte => leftValue <= rightValue
      case ExampleIntegerComparisonOperator.Gte => leftValue >= rightValue
  }

  override def visitExampleIntegerEqualComparisonOperation(node: ExampleIntegerEqualComparisonOperation, environment: ExampleEnvironment[Int | Boolean]): Try[Boolean] = {
    for {
      case leftValue: Int <- node.left.accept(this, environment)
      case rightValue: Int <- node.right.accept(this, environment)
    } yield node.operator match
      case ExampleEqualComparisonOperator.Eq => leftValue == rightValue
      case ExampleEqualComparisonOperator.Ne => leftValue != rightValue
  }
}
