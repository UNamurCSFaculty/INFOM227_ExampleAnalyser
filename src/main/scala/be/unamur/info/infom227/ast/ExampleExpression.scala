package be.unamur.info.infom227.ast

import be.unamur
import be.unamur.info
import be.unamur.info.infom227
import be.unamur.info.infom227.*

sealed trait ExampleExpression extends ExampleAstNode {
  def exampleType: ExampleType
}


sealed trait ExampleIntegerExpression extends ExampleExpression {
  override def exampleType: ExampleType = ExampleInt
}

case class ExampleIntegerConstant(value: Int) extends ExampleIntegerExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleIntegerConstant(this, environment)
  }
}

case class ExampleIntegerVariable(name: String) extends ExampleIntegerExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleIntegerVariable(this, environment)
  }
}

case class ExampleIntegerUnaryOperation(operator: ExampleIntegerUnaryOperator, value: ExampleIntegerExpression) extends ExampleIntegerExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleIntegerUnaryOperation(this, environment)
  }
}

case class ExampleIntegerBinaryOperation(operator: ExampleIntegerBinaryOperator, left: ExampleIntegerExpression, right: ExampleIntegerExpression) extends ExampleIntegerExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleIntegerBinaryOperation(this, environment)
  }
}


sealed trait ExampleBooleanExpression extends ExampleExpression {
  override def exampleType: ExampleType = ExampleBool

  def negate: ExampleBooleanExpression
}

case class ExampleBooleanConstant(value: Boolean) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleBooleanConstant(this, environment)
  }

  override def negate: ExampleBooleanConstant = ExampleBooleanConstant(!value)
}

case class ExampleBooleanVariable(name: String) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleBooleanVariable(this, environment)
  }

  override def negate: ExampleBooleanExpression = ExampleBooleanUnaryOperation(ExampleBooleanUnaryOperator.Neg, ExampleBooleanVariable(name))
}

case class ExampleBooleanUnaryOperation(operator: ExampleBooleanUnaryOperator, value: ExampleBooleanExpression) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleBooleanUnaryOperation(this, environment)
  }

  override def negate: ExampleBooleanExpression = operator match
    case ExampleBooleanUnaryOperator.Neg => value
}

case class ExampleBooleanBinaryOperation(operator: ExampleBooleanBinaryOperator, left: ExampleBooleanExpression, right: ExampleBooleanExpression) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleBooleanBinaryOperation(this, environment)
  }

  override def negate: ExampleBooleanExpression = operator match
    case ExampleBooleanBinaryOperator.And => ExampleBooleanBinaryOperation(ExampleBooleanBinaryOperator.Or, left.negate, right.negate)
    case ExampleBooleanBinaryOperator.Or => ExampleBooleanBinaryOperation(ExampleBooleanBinaryOperator.And, left.negate, right.negate)
}

case class ExampleBooleanEqualComparisonOperation(operator: ExampleEqualComparisonOperator, left: ExampleBooleanExpression, right: ExampleBooleanExpression) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleBooleanEqualComparisonOperation(this, environment)
  }

  override def negate: ExampleBooleanExpression = operator match
    case ExampleEqualComparisonOperator.Eq => ExampleBooleanEqualComparisonOperation(ExampleEqualComparisonOperator.Ne, left, right)
    case ExampleEqualComparisonOperator.Ne => ExampleBooleanEqualComparisonOperation(ExampleEqualComparisonOperator.Eq, left, right)
}

case class ExampleIntegerComparisonOperation(operator: ExampleIntegerComparisonOperator, left: ExampleIntegerExpression, right: ExampleIntegerExpression) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleIntegerComparisonOperation(this, environment)
  }

  override def negate: ExampleBooleanExpression = operator match
    case ExampleIntegerComparisonOperator.Gt => ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Lte, left, right)
    case ExampleIntegerComparisonOperator.Lt => ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Gte, left, right)
    case ExampleIntegerComparisonOperator.Gte => ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Lt, left, right)
    case ExampleIntegerComparisonOperator.Lte => ExampleIntegerComparisonOperation(ExampleIntegerComparisonOperator.Gt, left, right)
}

case class ExampleIntegerEqualComparisonOperation(operator: ExampleEqualComparisonOperator, left: ExampleIntegerExpression, right: ExampleIntegerExpression) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleIntegerEqualComparisonOperation(this, environment)
  }

  override def negate: ExampleBooleanExpression = operator match
    case ExampleEqualComparisonOperator.Eq => ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Ne, left, right)
    case ExampleEqualComparisonOperator.Ne => ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Eq, left, right)
}
