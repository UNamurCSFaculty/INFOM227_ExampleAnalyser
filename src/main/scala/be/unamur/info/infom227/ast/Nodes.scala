package be.unamur.info.infom227.ast

// Operators

enum EqualComparisonOperator {
  case Eq
  case Ne

  def negate: EqualComparisonOperator = this match {
    case Eq => Ne
    case Ne => Eq
  }
}

enum IntegerBinaryOperator {
  case Add
  case Sub
  case Mul
  case Div
}

enum IntegerComparisonOperator {
  case Gt
  case Lt
  case Gte
  case Lte

  def negate: IntegerComparisonOperator = this match {
    case Gt => Lte
    case Lt => Gte
    case Gte => Lt
    case Lte => Gt
  }
}

enum BooleanBinaryOperator {
  case And
  case Or

  def negate: BooleanBinaryOperator = this match {
    case And => Or
    case Or => And
  }
}

// Expressions

sealed trait Expression


sealed trait ArithmeticExpression extends Expression

case class ArithmeticConstant(value: Int) extends ArithmeticExpression

case class ArithmeticBinaryOperation(left: ArithmeticOperand, operator: IntegerBinaryOperator, right: ArithmeticOperand) extends ArithmeticExpression


sealed trait BooleanExpression extends Expression {
  def negate: BooleanExpression
}

case class BooleanConstant(value: Boolean) extends BooleanExpression {
  override def negate: BooleanConstant = BooleanConstant(!value)
}

case class BooleanNegOperation(value: BooleanOperand) extends BooleanExpression {
  override def negate: BooleanOperand = value
}

case class BooleanBinaryOperation(left: BooleanOperand, operator: BooleanBinaryOperator | EqualComparisonOperator, right: BooleanOperand) extends BooleanExpression {

  override def negate: BooleanExpression = operator match
    case booleanBinaryOperator: BooleanBinaryOperator => BooleanBinaryOperation(negateBooleanOperand(left), booleanBinaryOperator.negate, negateBooleanOperand(right))
    case equalComparisonOperator: EqualComparisonOperator => BooleanBinaryOperation(left, equalComparisonOperator.negate, right)
}

case class IntegerComparisonOperation(left: ArithmeticOperand, operator: IntegerComparisonOperator | EqualComparisonOperator, right: ArithmeticOperand) extends BooleanExpression {
  override def negate: BooleanExpression = operator match {
    case integerComparisonOperator: IntegerComparisonOperator => IntegerComparisonOperation(left, integerComparisonOperator.negate, right)
    case equalComparisonOperator: EqualComparisonOperator => IntegerComparisonOperation(left, equalComparisonOperator.negate, right)
  }
}


case class Variable(name: String) extends ArithmeticExpression, BooleanExpression {
  override def negate: BooleanNegOperation = BooleanNegOperation(Variable(name))
}


type ArithmeticOperand = ArithmeticConstant | Variable

type BooleanOperand = BooleanConstant | BooleanNegOperation | Variable

def negateBooleanOperand(operand: BooleanOperand): BooleanOperand = operand match {
  case booleanConstant: BooleanConstant => booleanConstant.negate
  case booleanNegOperation: BooleanNegOperation => booleanNegOperation.negate
  case variable: Variable => variable.negate
}

// Call

case class FunctionCall(name: String, arguments: List[Expression])

// Statements

sealed trait Statement {
  def lineNumber: Int
}

case class AssignStatement(lineNumber: Int, variable: String, expression: Expression | FunctionCall) extends Statement

case class IfStatement(lineNumber: Int, condition: BooleanExpression, ifBody: List[Statement], elseBody: List[Statement]) extends Statement

case class WhileStatement(lineNumber: Int, condition: BooleanExpression, body: List[Statement]) extends Statement

case class ReturnStatement(lineNumber: Int, expression: Expression) extends Statement

// Program

case class Function(lineNumber: Int, parameters: List[String], body: List[Statement])

case class Program(functions: Map[String, Function])
