package be.unamur.info.infom227.cfg

import be.unamur.info.infom227.ast.*
import be.unamur.info.infom227.cst.ExampleParser
import be.unamur.info.infom227.cfg.FromLineNumber.pp
import org.antlr.v4.runtime.CharStreams
import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}


class TestExampleCfgBuilder extends AnyFunSuite {

  test("convert a simple example AST") {
    val code =
      """
    int a;
    a = 3 + 2 * 2;
    print a;
    """

    val charStream = CharStreams.fromString(code)

    val tryAst = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    val ast = tryAst match
      case Success(ast) => ast
      case Failure(exception) => fail(exception)

    val expected = ExampleCfg(
      Map(
        (pp(ast, 2), pp(ast, 3)) -> ExampleBooleanConstant(true),
        (pp(ast, 3), pp(ast, 4)) -> ExampleBooleanConstant(true),
      )
    )

    val cfg = ExampleCfgBuilder.build(ast)

    assert(expected == cfg)
  }

  test("convert a simple example AST with a while") {
    val code =
      """
    int a;
    while (a == 0) {
      a = 3 + 2 * 2;
    }
    print a;
    """

    val charStream = CharStreams.fromString(code)

    val tryAst = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    val ast = tryAst match
      case Success(ast) => ast
      case Failure(exception) => fail(exception)

    val expected = ExampleCfg(
      Map(
        (pp(ast, 2), pp(ast, 3)) -> ExampleBooleanConstant(true),
        (pp(ast, 3), pp(ast, 4)) -> ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Eq, ExampleIntegerVariable("a"), ExampleIntegerConstant(0)),
        (pp(ast, 4), pp(ast, 3)) -> ExampleBooleanConstant(true),
        (pp(ast, 3), pp(ast, 6)) -> ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Ne, ExampleIntegerVariable("a"), ExampleIntegerConstant(0))
      )
    )

    val cfg = ExampleCfgBuilder.build(ast)

    assert(expected == cfg)
  }


  test("convert a simple example AST with a if") {
    val code =
      """
    int a;
    if (a == 0) {
      a = 3 + 2 * 2;
    } else {
      a = 3 + 3 * 2;
    }
    print a;
    """

    val charStream = CharStreams.fromString(code)

    val tryAst = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
    } yield ast

    val ast = tryAst match
      case Success(ast) => ast
      case Failure(exception) => fail(exception)

    val expected = ExampleCfg(
      Map(
        (pp(ast, 2), pp(ast, 3)) -> ExampleBooleanConstant(true),
        (pp(ast, 3), pp(ast, 4)) -> ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Eq, ExampleIntegerVariable("a"), ExampleIntegerConstant(0)),
        (pp(ast, 3), pp(ast, 6)) -> ExampleIntegerEqualComparisonOperation(ExampleEqualComparisonOperator.Ne, ExampleIntegerVariable("a"), ExampleIntegerConstant(0)),
        (pp(ast, 4), pp(ast, 8)) -> ExampleBooleanConstant(true),
        (pp(ast, 6), pp(ast, 8)) -> ExampleBooleanConstant(true),
      )
    )

    val cfg = ExampleCfgBuilder.build(ast)

    assert(expected == cfg)
  }
}
