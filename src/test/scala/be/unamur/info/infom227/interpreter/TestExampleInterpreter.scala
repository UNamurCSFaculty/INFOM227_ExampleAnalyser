package be.unamur.info.infom227.interpreter

import be.unamur.info.infom227.ast.ExampleAstBuilder
import be.unamur.info.infom227.cst.ExampleParser
import org.antlr.v4.runtime.CharStreams
import org.scalatest.funsuite.AnyFunSuite

import java.io.{BufferedWriter, StringWriter}
import scala.util.Success

class TestExampleInterpreter extends AnyFunSuite {

  test("run a simple example AST") {
    val code =
      """
    int a;
    a = 3 + 2 * 2;
    print a;
    """

    val expected = "7\n"

    val charStream = CharStreams.fromString(code)

    val stringWriter = StringWriter()

    val environment = ExampleEnvironment[Int | Boolean](BufferedWriter(stringWriter))

    val result = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
      result <- ExampleInterpreter.run(ast, environment)
    } yield result

    assert(Success(()) == result)
    assert(expected == stringWriter.toString)
  }

  test("print a boolean expression") {
    val code =
      """
    print 2 * 2 > 3;
    """

    val expected = "True\n"

    val charStream = CharStreams.fromString(code)

    val stringWriter = StringWriter()

    val environment = ExampleEnvironment[Int | Boolean](BufferedWriter(stringWriter))

    val result = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
      result <- ExampleInterpreter.run(ast, environment)
    } yield result

    assert(Success(()) == result)
    assert(expected == stringWriter.toString)
  }

  test("print in a scope") {
    val code =
      """
    int a;
    a = 1;
    a = {
      int b;
      b = 3;
      print a + b;
      b
    };
    print a;
    """

    val expected = "4\n3\n"

    val charStream = CharStreams.fromString(code)

    val stringWriter = StringWriter()

    val environment = ExampleEnvironment[Int | Boolean](BufferedWriter(stringWriter))

    val result = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
      result <- ExampleInterpreter.run(ast, environment)
    } yield result

    assert(Success(()) == result)
    assert(expected == stringWriter.toString)
  }

  test("print div by zero") {
    val code =
      """
    print 10 / 0;
    """

    val expected = ""

    val charStream = CharStreams.fromString(code)

    val stringWriter = StringWriter()

    val environment = ExampleEnvironment[Int | Boolean](BufferedWriter(stringWriter))

    val result = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
      result <- ExampleInterpreter.run(ast, environment)
    } yield result

    assert(result.isFailure)
    assert(expected == stringWriter.toString)
  }

  test("print in a if") {
    val code =
      """
    if (10 > 5) {
      print 1;
    } else {
      print 2;
    }
    """

    val expected = "1\n"

    val charStream = CharStreams.fromString(code)

    val stringWriter = StringWriter()

    val environment = ExampleEnvironment[Int | Boolean](BufferedWriter(stringWriter))

    val result = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
      result <- ExampleInterpreter.run(ast, environment)
    } yield result

    assert(Success(()) == result)
    assert(expected == stringWriter.toString)
  }

  test("print in a else") {
    val code =
      """
    if (10 < 5) {
      print 1;
    } else {
      print 2;
    }
    """

    val expected = "2\n"

    val charStream = CharStreams.fromString(code)

    val stringWriter = StringWriter()

    val environment = ExampleEnvironment[Int | Boolean](BufferedWriter(stringWriter))

    val result = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
      result <- ExampleInterpreter.run(ast, environment)
    } yield result

    assert(Success(()) == result)
    assert(expected == stringWriter.toString)
  }

  test("print in a while") {
    val code =
      """
    int i;
    i = 0;
    while (i < 3) {
      print i;
      i = i + 1;
    }
    """

    val expected = "0\n1\n2\n"

    val charStream = CharStreams.fromString(code)

    val stringWriter = StringWriter()

    val environment = ExampleEnvironment[Int | Boolean](BufferedWriter(stringWriter))

    val result = for {
      cst <- ExampleParser.parse(charStream)
      ast <- ExampleAstBuilder.build(cst)
      result <- ExampleInterpreter.run(ast, environment)
    } yield result

    assert(Success(()) == result)
    assert(expected == stringWriter.toString)
  }
}
