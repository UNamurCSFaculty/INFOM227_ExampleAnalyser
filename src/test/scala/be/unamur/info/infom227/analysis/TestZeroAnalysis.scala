package be.unamur.info.infom227.analysis

import be.unamur.info.infom227.{ast, cfg, cst}
import org.antlr.v4.runtime.CharStreams
import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

class TestZeroAnalysis extends AnyFunSuite {
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
      main:
        a: NonZero
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
      main:
        a: Top
        b: Top
        x: NonZero
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
      main:
        i: NonZero
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
      main:
        i: NonZero
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
      main:
        i: NonZero
      """,
    )
  ).foreach { (name, code, expectedAnalysis) =>
    test(name) {
      val charStream = CharStreams.fromString(code)

      val tryAnalyses = for {
        programContext <- cst.parse(charStream)
        program <- ast.build(programContext)
        cfgs = cfg.build(program)
        analyses <- analyseProgram(cfgs, DummyObserver())
      } yield analyses

      val analyses = tryAnalyses match {
        case Success(analyses) => analyses
        case Failure(exception) => fail(exception)
      }

      val builder = new StringBuilder
      for ((functionName, analysis) <- analyses) {
        builder.append(s"      $functionName:\n")
        for {line <- analysis.toString.split("\n")} {
          builder.append(s"        $line\n")
        }
      }
      val actualAnalysis = builder.toString()

      assert(expectedAnalysis.strip == actualAnalysis.strip, s"Expected:\n      ${expectedAnalysis.strip}\nActual:\n      ${actualAnalysis.strip}")
    }
  }
}
