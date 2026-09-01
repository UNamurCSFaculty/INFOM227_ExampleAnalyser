package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.analysis.{DummyObserver, zeroAnalysis}
import be.unamur.info.infom227.small.cfg.ProgramPoint
import be.unamur.info.infom227.small.{ast, cfg, cst}
import org.antlr.v4.runtime.CharStreams
import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success}

class TestZeroAnalysis extends AnyFunSuite {
  Seq(
    (
      "analyse a simple AST with a single zero assignment",
      """
      function main() {
          a = 0;
          return a;
      }
      """,
      """
      main:
        a: Zero
      """,
    ),
    (
      "analyse a simple AST with a single non-zero assignment",
      """
      function main() {
          a = 3;
          return a;
      }
      """,
      """
      main:
        a: NonZero
      """,
    ),
    (
      "analyse a simple AST with multiple assignments",
      """
      function main() {
          a = 0;
          a = a + 5;
          b = 7 - a;
          x = 10 + 2;
          return 0;
      }
      """,
      """
      main:
        a: NonZero
        b: Unknown
        x: Unknown
      """,
    ),
    (
      "analyse a simple AST with a if statement",
      """
      function main() {
          i = 1;
          if (i < 10) {
              i = 1;
          } else {
              i = 2;
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
      "analyse a simple AST with a while statement",
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
      "analyse a simple AST with early return",
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
        analyses <- zeroAnalysis(cfgs, DummyObserver())
      } yield analyses

      val analyses = tryAnalyses match {
        case Success(analyses) => analyses
        case Failure(exception) => fail(exception)
      }

      val builder = new StringBuilder
      for ((functionName, analysis) <- analyses) {
        builder.append(s"      $functionName:\n")
        for {line <- analysis.abstractStates(ProgramPoint.ExitPoint).toString.split("\n")} {
          builder.append(s"        $line\n")
        }
      }
      val actualAnalysis = builder.toString()

      assert(expectedAnalysis.strip == actualAnalysis.strip, s"Expected:\n      ${expectedAnalysis.strip}\nActual:\n      ${actualAnalysis.strip}")
    }
  }

  Seq(
    (
      "interpret a division by zero",
      """
      function main() {
          a = 0;
          b = 5 / a;
          return 0;
      }
      """,
      """
      main:
        [Error] Division by zero at line 4
      """,
    ),
    (
      "interpret a potential division by zero",
      """
      function main() {
          if (5 == 3) {
              a = 0;
          } else {
              a = 5;
          }
          b = 5 / a;
          return 0;
      }
      """,
      """
      main:
        [Warning] Potential division by zero at line 8
      """,
    )
  ).foreach { (name, code, expectedAnalysis) =>
    test(name) {
      val charStream = CharStreams.fromString(code)

      val tryAnalyses = for {
        programContext <- cst.parse(charStream)
        program <- ast.build(programContext)
        cfgs = cfg.build(program)
        zeroAnalyses <- zeroAnalysis(cfgs, DummyObserver())
        analyses <- zeroAnalysisInterpreter(cfgs, zeroAnalyses, DummyObserver())
      } yield analyses

      val analyses = tryAnalyses match {
        case Success(analyses) => analyses
        case Failure(exception) => fail(exception)
      }

      val builder = new StringBuilder
      for ((functionName, analysis) <- analyses) {
        builder.append(s"      $functionName:\n")
        for {(diagnosticType, message) <- analysis.diagnostics} {
          builder.append(s"        [$diagnosticType] $message\n")
        }
      }
      val actualAnalysis = builder.toString()

      assert(expectedAnalysis.strip == actualAnalysis.strip, s"Expected:\n      ${expectedAnalysis.strip}\nActual:\n      ${actualAnalysis.strip}")
    }
  }
}
