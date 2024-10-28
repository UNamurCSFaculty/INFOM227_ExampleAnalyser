package be.unamur.info.infom227.ast

import be.unamur.info.infom227.cst.ExampleParser
import org.antlr.v4.runtime.CharStreams
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Success

class TestExampleAstBuilder extends AnyFunSuite {

  test("build simple example AST") {
    val code =
      """
    int a;
    int b;
    a = 3 * 4 / 5;
    b = -a + b - 7;
    """

    val expected = Success(
      ExampleProgram(
        ExampleScope(
          ExampleDeclareStatement("a", ExampleInt),
          ExampleDeclareStatement("b", ExampleInt),
          ExampleAssignStatement("a", ExampleScope(), ExampleIntegerBinaryOperation(
            ExampleIntegerBinaryOperator.Div,
            ExampleIntegerBinaryOperation(
              ExampleIntegerBinaryOperator.Mul,
              ExampleIntegerConstant(3),
              ExampleIntegerConstant(4)
            ),
            ExampleIntegerConstant(5)
          )),
          ExampleAssignStatement("b", ExampleScope(), ExampleIntegerBinaryOperation(
            ExampleIntegerBinaryOperator.Sub,
            ExampleIntegerBinaryOperation(
              ExampleIntegerBinaryOperator.Add,
              ExampleIntegerUnaryOperation(
                ExampleIntegerUnaryOperator.Neg,
                ExampleIntegerVariable("a")
              ),
              ExampleIntegerVariable("b")
            ),
            ExampleIntegerConstant(7)
          )),
        )
      )
    )

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(expected == ast)
  }

  test("build simple example AST with scope") {
    val code =
      """
    int a;
    a = {
      int b;
      b = -a + b - 7;
      3 * 4 / b
    };
    """

    val expected = Success(
      ExampleProgram(
        ExampleScope(
          ExampleDeclareStatement("a", ExampleInt),
          ExampleAssignStatement(
            "a",
            ExampleScope(
              ExampleDeclareStatement("b", ExampleInt),
              ExampleAssignStatement("b", ExampleScope(), ExampleIntegerBinaryOperation(
                ExampleIntegerBinaryOperator.Sub,
                ExampleIntegerBinaryOperation(
                  ExampleIntegerBinaryOperator.Add,
                  ExampleIntegerUnaryOperation(
                    ExampleIntegerUnaryOperator.Neg,
                    ExampleIntegerVariable("a")
                  ),
                  ExampleIntegerVariable("b")
                ),
                ExampleIntegerConstant(7)
              )),
            ),
            ExampleIntegerBinaryOperation(
              ExampleIntegerBinaryOperator.Div,
              ExampleIntegerBinaryOperation(
                ExampleIntegerBinaryOperator.Mul,
                ExampleIntegerConstant(3),
                ExampleIntegerConstant(4)
              ),
              ExampleIntegerVariable("b")
            )
          ),
        )
      )
    )

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(expected == ast)
  }

  test("build simple example AST with operator priority") {
    val code =
      """
    int a;
    a = 2*2 + 3*3;
    """

    val expected = Success(
      ExampleProgram(
        ExampleScope(
          ExampleDeclareStatement("a", ExampleInt),
          ExampleAssignStatement("a", ExampleScope(), ExampleIntegerBinaryOperation(
            ExampleIntegerBinaryOperator.Add,
            ExampleIntegerBinaryOperation(
              ExampleIntegerBinaryOperator.Mul,
              ExampleIntegerConstant(2),
              ExampleIntegerConstant(2)
            ),
            ExampleIntegerBinaryOperation(
              ExampleIntegerBinaryOperator.Mul,
              ExampleIntegerConstant(3),
              ExampleIntegerConstant(3)
            ),
          ))
        )
      )
    )

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(expected == ast)
  }

  test("build simple example AST with a comparison") {
    val code =
      """
    int a;
    bool b;
    a = 1 + 2;
    b = not (a == 5);
    """

    val expected = Success(
      ExampleProgram(
        ExampleScope(
          ExampleDeclareStatement("a", ExampleInt),
          ExampleDeclareStatement("b", ExampleBool),
          ExampleAssignStatement("a", ExampleScope(), ExampleIntegerBinaryOperation(
            ExampleIntegerBinaryOperator.Add,
            ExampleIntegerConstant(1),
            ExampleIntegerConstant(2)
          )),
          ExampleAssignStatement("b", ExampleScope(), ExampleBooleanUnaryOperation(
            ExampleBooleanUnaryOperator.Neg,
            ExampleIntegerEqualComparisonOperation(
              ExampleEqualComparisonOperator.Eq,
              ExampleIntegerVariable("a"),
              ExampleIntegerConstant(5)
            )
          )),
        )
      )
    )

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(expected == ast)
  }

  test("build simple example AST with a if statement") {
    val code =
      """
    if (True) {
      print 1;
    } else {
      print 2;
    }
    """

    val expected = Success(
      ExampleProgram(
        ExampleScope(
          ExampleIfStatement(
            ExampleBooleanConstant(true),
            ExampleStatements(
              ExamplePrintStatement(
                ExampleIntegerConstant(1)
              )
            ),
            ExampleStatements(
              ExamplePrintStatement(
                ExampleIntegerConstant(2)
              )
            )
          )
        )
      )
    )

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(expected == ast)
  }

  test("build simple example AST with a while statement") {
    val code =
      """
    while (True) {
      print 1;
    }
    """

    val expected = Success(
      ExampleProgram(
        ExampleScope(
          ExampleWhileStatement(
            ExampleBooleanConstant(true),
            ExampleStatements(
              ExamplePrintStatement(
                ExampleIntegerConstant(1)
              )
            ),
          )
        )
      )
    )

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(expected == ast)
  }

  test("fail to build a simple example AST because of an undefined variable") {
    val code =
      """
    int a;
    a = b * 2;
    """

    val charStream = CharStreams.fromString(code)

    val ast = for {
      tree <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(tree)
    } yield ast

    assert(ast.isFailure)
  }

  test("fail to build a simple example AST because of an assignment from scope") {
    val code =
      """
    int a;
    int b;
    a = {
      b = 2;
      4
    };
    """

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(ast.isFailure)
  }

  test("fail to build a simple example AST because of an access to a variable from a scope outside the scope") {
    val code =
      """
    int a;
    a = {
      int b;
      b = 2;
      4
    };
    a = a + b;
    """

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(ast.isFailure)
  }

  test("fail to build a simple example AST because of an assignment of a boolean expression to an integer variable") {
    val code =
      """
    int a;
    a = True and False;
    """

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(ast.isFailure)
  }

  test("fail to build a simple example AST because the negation of a boolean variable") {
    val code =
      """
    int a;
    a = - True;
    """

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(ast.isFailure)
  }

  test("fail to build a simple example AST because of a boolean in a integer expression") {
    val code =
      """
    int a;
    a = 5 + True;
    """

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(ast.isFailure)
  }

  test("fail to build a simple example AST because of a integer in a boolean expression") {
    val code =
      """
    int a;
    a = 5 and True;
    """

    val charStream = CharStreams.fromString(code)

    val ast = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    assert(ast.isFailure)
  }
}
