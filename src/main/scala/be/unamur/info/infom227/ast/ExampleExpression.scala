package be.unamur.info.infom227.ast

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
}

case class ExampleBooleanConstant(value: Boolean) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleBooleanConstant(this, environment)
  }
}

case class ExampleBooleanVariable(name: String) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleBooleanVariable(this, environment)
  }
}

case class ExampleBooleanUnaryOperation(operator: ExampleBooleanUnaryOperator, value: ExampleBooleanExpression) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleBooleanUnaryOperation(this, environment)
  }
}

case class ExampleBooleanBinaryOperation(operator: ExampleBooleanBinaryOperator, left: ExampleBooleanExpression, right: ExampleBooleanExpression) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleBooleanBinaryOperation(this, environment)
  }
}

case class ExampleBooleanEqualComparisonOperation(operator: ExampleEqualComparisonOperator, left: ExampleBooleanExpression, right: ExampleBooleanExpression) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleBooleanEqualComparisonOperation(this, environment)
  }
}

case class ExampleIntegerComparisonOperation(operator: ExampleIntegerComparisonOperator, left: ExampleIntegerExpression, right: ExampleIntegerExpression) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleIntegerComparisonOperation(this, environment)
  }
}

case class ExampleIntegerEqualComparisonOperation(operator: ExampleEqualComparisonOperator, left: ExampleIntegerExpression, right: ExampleIntegerExpression) extends ExampleBooleanExpression {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleIntegerEqualComparisonOperation(this, environment)
  }
}
