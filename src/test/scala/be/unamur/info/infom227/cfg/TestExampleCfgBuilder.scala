package be.unamur.info.infom227.cfg

import be.unamur.info.infom227.ast.*
import be.unamur.info.infom227.cst.ExampleParser
import org.antlr.v4.runtime.CharStreams
import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

private class FromLineNumber extends ExampleSequenceVisitor[Option[ExampleStatement], Int], ExampleStatementVisitor[Option[ExampleStatement], Int] {

  override def visitExampleProgram(node: ExampleProgram, lineNumber: Int): Option[ExampleStatement] = {
    visitExampleScope(node.scope, lineNumber)
  }

  private def visitSequence(statements: Seq[ExampleStatement], lineNumber: Int): Option[ExampleStatement] = {
    statements.foldLeft[Option[ExampleStatement]](None) {
      (acc, statement) => acc.orElse(statement.accept(this, lineNumber))
    }
  }

  override def visitExampleScope(node: ExampleScope, lineNumber: Int): Option[ExampleStatement] = {
    visitSequence(node.statements, lineNumber)
  }

  override def visitExampleStatements(node: ExampleStatements, lineNumber: Int): Option[ExampleStatement] = {
    visitSequence(node.statements, lineNumber)
  }

  private def visitStatement(node: ExampleStatement, lineNumber: Int): Option[ExampleStatement] = {
    if (node.lineNumber == lineNumber) {
      Some(node)
    } else {
      None
    }
  }

  override def visitExampleDeclareStatement(node: ExampleDeclareStatement, lineNumber: Int): Option[ExampleStatement] = {
    visitStatement(node, lineNumber)
  }

  override def visitExampleAssignStatement(node: ExampleAssignStatement, lineNumber: Int): Option[ExampleStatement] = {
    visitStatement(node, lineNumber)
  }

  override def visitExamplePrintStatement(node: ExamplePrintStatement, lineNumber: Int): Option[ExampleStatement] = {
    visitStatement(node, lineNumber)
  }

  override def visitExampleIfStatement(node: ExampleIfStatement, lineNumber: Int): Option[ExampleStatement] = {
    visitStatement(node, lineNumber)
      .orElse(visitSequence(node.ifStatements.statements, lineNumber))
      .orElse(visitSequence(node.elseStatements.statements, lineNumber))
  }

  override def visitExampleWhileStatement(node: ExampleWhileStatement, lineNumber: Int): Option[ExampleStatement] = {
    visitStatement(node, lineNumber).orElse(visitSequence(node.statements.statements, lineNumber))
  }
}

class TestExampleCfgBuilder extends AnyFunSuite {

  def pp(program: ExampleProgram, lineNumber: Int): ExampleProgramPoint = {
    FromLineNumber().visitExampleProgram(program, lineNumber) match
      case Some(statement) => ExampleProgramPoint(statement)
      case None => fail(s"Unknown program line: $lineNumber")
  }

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
