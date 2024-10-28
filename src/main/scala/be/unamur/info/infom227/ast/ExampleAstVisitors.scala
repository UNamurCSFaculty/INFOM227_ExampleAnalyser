package be.unamur.info.infom227.ast

import be.unamur.info.infom227.*

trait ExampleSequenceVisitor[T, D] {
  def visitExampleProgram(node: ExampleProgram, data: D): T

  def visitExampleScope(node: ExampleScope, data: D): T

  def visitExampleStatements(node: ExampleStatements, data: D): T
}

trait ExampleStatementVisitor[T, D] {
  def visitExampleDeclareStatement(node: ExampleDeclareStatement, data: D): T

  def visitExampleAssignStatement(node: ExampleAssignStatement, data: D): T

  def visitExamplePrintStatement(node: ExamplePrintStatement, data: D): T

  def visitExampleIfStatement(node: ExampleIfStatement, data: D): T

  def visitExampleWhileStatement(node: ExampleWhileStatement, data: D): T
}

trait ExampleTypeVisitor[T, D] {
  def visitExampleInt(node: ExampleInt.type, data: D): T

  def visitExampleBool(node: ExampleBool.type, data: D): T
}

trait ExampleExpressionVisitor[T, D] {
  def visitExampleIntegerConstant(node: ExampleIntegerConstant, data: D): T

  def visitExampleIntegerVariable(node: ExampleIntegerVariable, data: D): T

  def visitExampleIntegerUnaryOperation(node: ExampleIntegerUnaryOperation, data: D): T

  def visitExampleIntegerBinaryOperation(node: ExampleIntegerBinaryOperation, data: D): T

  def visitExampleBooleanConstant(node: ExampleBooleanConstant, data: D): T

  def visitExampleBooleanVariable(node: ExampleBooleanVariable, data: D): T

  def visitExampleBooleanUnaryOperation(node: ExampleBooleanUnaryOperation, data: D): T

  def visitExampleBooleanBinaryOperation(node: ExampleBooleanBinaryOperation, data: D): T

  def visitExampleBooleanEqualComparisonOperation(node: ExampleBooleanEqualComparisonOperation, data: D): T

  def visitExampleIntegerComparisonOperation(node: ExampleIntegerComparisonOperation, data: D): T

  def visitExampleIntegerEqualComparisonOperation(node: ExampleIntegerEqualComparisonOperation, data: D): T
}
