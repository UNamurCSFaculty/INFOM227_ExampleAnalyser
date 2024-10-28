package be.unamur.info.infom227.ast

sealed trait ExampleType {
  def accept[T, E](visitor: ExampleTypeVisitor[T, E], environment: E): T
}

case object ExampleInt extends ExampleType {
  override def accept[T, E](visitor: ExampleTypeVisitor[T, E], environment: E): T = {
    visitor.visitExampleInt(this, environment)
  }
}

case object ExampleBool extends ExampleType {
  override def accept[T, E](visitor: ExampleTypeVisitor[T, E], environment: E): T = {
    visitor.visitExampleBool(this, environment)
  }
}
