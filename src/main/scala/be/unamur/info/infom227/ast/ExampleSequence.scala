package be.unamur.info.infom227.ast

trait ExampleSequence {
  def accept[T, E](visitor: ExampleSequenceVisitor[T, E], environment: E): T
}

case class ExampleProgram(scope: ExampleScope) extends ExampleSequence {
  override def accept[T, E](visitor: ExampleSequenceVisitor[T, E], environment: E): T = {
    visitor.visitExampleProgram(this, environment)
  }
}

case class ExampleScope(variables: Map[String, ExampleType], statements: ExampleStatement*) extends ExampleSequence {
  override def accept[T, E](visitor: ExampleSequenceVisitor[T, E], environment: E): T = {
    visitor.visitExampleScope(this, environment)
  }
}

case class ExampleStatements(statements: ExampleNonDeclareStatement*) extends ExampleSequence {
  override def accept[T, E](visitor: ExampleSequenceVisitor[T, E], environment: E): T = {
    visitor.visitExampleStatements(this, environment)
  }
}
