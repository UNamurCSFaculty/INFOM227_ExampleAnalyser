package be.unamur.info.infom227.ast

import be.unamur.info.infom227.*

trait ExampleAstVisitor[T, E] {
  def visitExampleProgram(node: ExampleProgram, environment: E): T

  def visitExampleScope(node: ExampleScope, environment: E): T

  def visitExampleStatements(node: ExampleStatements, environment: E): T

  def visitExampleDeclareStatement(node: ExampleDeclareStatement, environment: E): T

  def visitExampleAssignStatement(node: ExampleAssignStatement, environment: E): T

  def visitExamplePrintStatement(node: ExamplePrintStatement, environment: E): T

  def visitExampleIfStatement(node: ExampleIfStatement, environment: E): T

  def visitExampleWhileStatement(node: ExampleWhileStatement, environment: E): T

  def visitExampleInt(node: ExampleInt.type, environment: E): T

  def visitExampleBool(node: ExampleBool.type, environment: E): T

  def visitExampleIntegerConstant(node: ExampleIntegerConstant, environment: E): T

  def visitExampleIntegerVariable(node: ExampleIntegerVariable, environment: E): T

  def visitExampleIntegerUnaryOperation(node: ExampleIntegerUnaryOperation, environment: E): T

  def visitExampleIntegerBinaryOperation(node: ExampleIntegerBinaryOperation, environment: E): T

  def visitExampleBooleanConstant(node: ExampleBooleanConstant, environment: E): T

  def visitExampleBooleanVariable(node: ExampleBooleanVariable, environment: E): T

  def visitExampleBooleanUnaryOperation(node: ExampleBooleanUnaryOperation, environment: E): T

  def visitExampleBooleanBinaryOperation(node: ExampleBooleanBinaryOperation, environment: E): T

  def visitExampleBooleanEqualComparisonOperation(node: ExampleBooleanEqualComparisonOperation, environment: E): T

  def visitExampleIntegerComparisonOperation(node: ExampleIntegerComparisonOperation, environment: E): T

  def visitExampleIntegerEqualComparisonOperation(node: ExampleIntegerEqualComparisonOperation, environment: E): T
}
