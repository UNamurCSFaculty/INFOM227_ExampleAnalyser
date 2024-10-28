package be.unamur.info.infom227.ast

enum ExampleEqualComparisonOperator {
  case Eq extends ExampleEqualComparisonOperator
  case Ne extends ExampleEqualComparisonOperator
}

enum ExampleIntegerUnaryOperator {
  case Pos extends ExampleIntegerUnaryOperator
  case Neg extends ExampleIntegerUnaryOperator
}

enum ExampleIntegerBinaryOperator {
  case Add extends ExampleIntegerBinaryOperator
  case Sub extends ExampleIntegerBinaryOperator
  case Mul extends ExampleIntegerBinaryOperator
  case Div extends ExampleIntegerBinaryOperator
}

enum ExampleIntegerComparisonOperator {
  case Gt extends ExampleIntegerComparisonOperator
  case Lt extends ExampleIntegerComparisonOperator
  case Gte extends ExampleIntegerComparisonOperator
  case Lte extends ExampleIntegerComparisonOperator
}

enum ExampleBooleanUnaryOperator {
  case Neg extends ExampleBooleanUnaryOperator
}

enum ExampleBooleanBinaryOperator {
  case And extends ExampleBooleanBinaryOperator
  case Or extends ExampleBooleanBinaryOperator
}
