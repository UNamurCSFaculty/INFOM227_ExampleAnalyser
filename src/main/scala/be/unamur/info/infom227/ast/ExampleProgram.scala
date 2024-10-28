package be.unamur.info.infom227.ast

case class ExampleProgram(scope: ExampleScope) extends ExampleAstNode {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleProgram(this, environment)
  }
}

case class ExampleScope(statements: ExampleStatement*) extends ExampleAstNode {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleScope(this, environment)
  }
}

case class ExampleStatements(statements: ExampleNonDeclareStatement*) extends ExampleAstNode {
  override def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T = {
    visitor.visitExampleStatements(this, environment)
  }
}
