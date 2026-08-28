package be.unamur.info.infom227.small.cfg

import be.unamur.info.infom227.small.cfg.build
import be.unamur.info.infom227.small.cst.parse
import be.unamur.info.infom227.small.{ast, cfg, cst}
import org.antlr.v4.runtime.CharStreams
import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

class TestCfgBuilder extends AnyFunSuite {
  Seq(
    (
      "convert a simple AST with a single assignment",
      """
      function main() {
          a = 3 + 2;
          return a;
      }
      """,
      """
      digraph "main" {
          "PP(3)" -> "PP(4)" [label="BooleanConstant(true)"];
          "EntryPoint" -> "PP(3)" [label="BooleanConstant(true)"];
          "PP(4)" -> "ExitPoint" [label="BooleanConstant(true)"];
      }
      """,
    ),
    (
      "convert a simple AST with multiple assignments",
      """
      function main() {
          a = 3 * 4;
          a = a / 5;
          b = 7 - a;
          x = 10 + 2;
          return 0;
      }
      """,
      """
      digraph "main" {
          "PP(3)" -> "PP(4)" [label="BooleanConstant(true)"];
          "PP(5)" -> "PP(6)" [label="BooleanConstant(true)"];
          "PP(7)" -> "ExitPoint" [label="BooleanConstant(true)"];
          "EntryPoint" -> "PP(3)" [label="BooleanConstant(true)"];
          "PP(6)" -> "PP(7)" [label="BooleanConstant(true)"];
          "PP(4)" -> "PP(5)" [label="BooleanConstant(true)"];
      }
      """,
    ),
    (
      "convert a simple AST with a if statement",
      """
      function main() {
          i = 1;
          if (i < 10) {
              i = i + 1;
          } else {
              i = i - 1;
          }
          return i;
      }
      """,
      """
      digraph "main" {
          "PP(7)" -> "PP(9)" [label="BooleanConstant(true)"];
          "PP(4)" -> "PP(5)" [label="IntegerComparisonOperation(Variable(i),Lt,ArithmeticConstant(10))"];
          "PP(3)" -> "PP(4)" [label="BooleanConstant(true)"];
          "PP(4)" -> "PP(7)" [label="IntegerComparisonOperation(Variable(i),Gte,ArithmeticConstant(10))"];
          "EntryPoint" -> "PP(3)" [label="BooleanConstant(true)"];
          "PP(9)" -> "ExitPoint" [label="BooleanConstant(true)"];
          "PP(5)" -> "PP(9)" [label="BooleanConstant(true)"];
      }
      """,
    ),
    (
      "convert a simple AST with a while statement",
      """
      function main() {
          i = 1;
          while (i < 10) {
              i = i + 1;
          }
          return i;
      }
      """,
      """
      digraph "main" {
          "PP(7)" -> "ExitPoint" [label="BooleanConstant(true)"];
          "PP(4)" -> "PP(7)" [label="IntegerComparisonOperation(Variable(i),Gte,ArithmeticConstant(10))"];
          "PP(3)" -> "PP(4)" [label="BooleanConstant(true)"];
          "EntryPoint" -> "PP(3)" [label="BooleanConstant(true)"];
          "PP(4)" -> "PP(5)" [label="IntegerComparisonOperation(Variable(i),Lt,ArithmeticConstant(10))"];
          "PP(5)" -> "PP(4)" [label="BooleanConstant(true)"];
      }
      """,
    ),
    (
      "convert a simple AST with early return",
      """
      function main() {
          i = 1;
          return i;
          a = 3 + 2;
          return a;
      }
      """,
      """
      digraph "main" {
          "PP(4)" -> "ExitPoint" [label="BooleanConstant(true)"];
          "PP(3)" -> "PP(4)" [label="BooleanConstant(true)"];
          "EntryPoint" -> "PP(3)" [label="BooleanConstant(true)"];
      }
      """,
    )
  ).foreach { (name, code, expectedDot) =>
    test(name) {
      val charStream = CharStreams.fromString(code)

      val tryProgram = for {
        programContext <- cst.parse(charStream)
        program <- ast.build(programContext)
      } yield program

      val program = tryProgram match {
        case Success(program) => program
        case Failure(exception) => fail(exception)
      }

      val cfgs = cfg.build(program)

      val builder = new StringBuilder
      for ((functionName, cfg) <- program.functions) {
        for {line <- cfgs(functionName).dot(functionName).split("\n")} {
          builder.append(s"      $line\n")
        }
      }
      val actualDot = builder.toString()

      assert(expectedDot.strip == actualDot.strip, s"Expected:\n      ${expectedDot.strip}\nActual:\n      ${actualDot.strip}")
    }
  }
}
