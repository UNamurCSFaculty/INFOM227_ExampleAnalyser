package be.unamur.info.infom227.small

import be.unamur.info.infom227.small.analysis.{DummyObserver, ZeroAnalysisObserver}
import be.unamur.info.infom227.small.ast.BuiltAstException
import be.unamur.info.infom227.small.cfg.ProgramPoint
import org.antlr.v4.runtime.CharStreams

import scala.util.{Failure, Success, Try}

val SUCCESS_ERROR_CODE = 0
val COMPILATION_ERROR_CODE = 1
val FATAL_ERROR_CODE = 2
val UNKNOWN_ACTION_ERROR_CODE = 3

@main def main(action: String, file: String, others: String*): Unit = {
  action match {
    case "run" =>
      val tryResult = for {
        charStream <- Try(CharStreams.fromFileName(file))
        programContext <- cst.parse(charStream)
        program <- ast.build(programContext)
        result <- interpreter.execute(program, "main")
      } yield result

      tryResult match
        case Success(returnValue) =>
          println(returnValue)
          System.exit(SUCCESS_ERROR_CODE)
        case Failure(exception: BuiltAstException) =>
          println(s"Compilation Error:\n${exception.getMessage}")
          System.exit(COMPILATION_ERROR_CODE)
        case Failure(exception: Throwable) =>
          println(s"Fatal error:\n${exception.getMessage}")
          System.exit(FATAL_ERROR_CODE)
    case "zero-analysis" =>
      val tryResult = for {
        charStream <- Try(CharStreams.fromFileName(file))
        programContext <- cst.parse(charStream)
        program <- ast.build(programContext)
        cfgs = cfg.build(program)
        zeroAnalyses <- analysis.zeroAnalysis(cfgs, if (others.contains("-v")) {
          ZeroAnalysisObserver()
        } else {
          DummyObserver()
        })
        moduleDiagnostics <- analysis.zeroAnalysisInterpreter(cfgs, zeroAnalyses, DummyObserver())
      } yield (zeroAnalyses, moduleDiagnostics)

      tryResult match {
        case Success((zeroAnalyses, moduleDiagnostics)) =>
          println("=====================================")
          println("            Zero Analysis            ")
          println("=====================================")
          for ((name, zeroAnalysis) <- zeroAnalyses) {
            println(s"Analysis for $name:")
            for (line <- zeroAnalysis.abstractStates(ProgramPoint.ExitPoint).toString().split("\n")) {
              println(s"  $line")
            }

            moduleDiagnostics.get(name).foreach(diagnostics =>
              for ((diagnosticType, message) <- diagnostics.diagnostics) {
                println(s"  [$diagnosticType] $message\n")
              }
            )
          }
          System.exit(SUCCESS_ERROR_CODE)
        case Failure(exception: BuiltAstException) =>
          println(s"Compilation Error:\n${exception.getMessage}")
          System.exit(COMPILATION_ERROR_CODE)
        case Failure(exception: Throwable) =>
          println(s"Fatal error:\n${exception.getMessage}")
          System.exit(FATAL_ERROR_CODE)
      }
    case action =>
      println(f"Unknown action: $action")
      System.exit(UNKNOWN_ACTION_ERROR_CODE)
  }
}
