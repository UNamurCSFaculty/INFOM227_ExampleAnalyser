package be.unamur.info.infom227.ast

sealed trait ExampleStatement extends ExampleAstNode {
  def lineNumber: Int
}

case class ExampleDeclareStatement(lineNumber: Int, name: String, exampleType: ExampleType) extends ExampleStatement {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleDeclareStatement(this, environment)
  }
}

sealed trait ExampleNonDeclareStatement extends ExampleStatement

case class ExampleAssignStatement(lineNumber: Int, variable: String, scope: ExampleScope, expression: ExampleExpression) extends ExampleNonDeclareStatement {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleAssignStatement(this, environment)
  }
}

case class ExamplePrintStatement(lineNumber: Int, expression: ExampleExpression) extends ExampleNonDeclareStatement {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExamplePrintStatement(this, environment)
  }
}

case class ExampleIfStatement(lineNumber: Int, condition: ExampleBooleanExpression, ifStatements: ExampleStatements, elseStatements: ExampleStatements) extends ExampleNonDeclareStatement {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleIfStatement(this, environment)
  }
}

case class ExampleWhileStatement(lineNumber: Int, condition: ExampleBooleanExpression, statements: ExampleStatements) extends ExampleNonDeclareStatement {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleWhileStatement(this, environment)
  }
}
