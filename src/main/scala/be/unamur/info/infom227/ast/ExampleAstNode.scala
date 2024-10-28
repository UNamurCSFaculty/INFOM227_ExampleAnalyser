package be.unamur.info.infom227.ast

trait ExampleAstNode {
  def accept[T, E](visitor: ExampleAstVisitor[T, E], environment: E): T
}
