package be.unamur.info.infom227.small

import be.unamur.info.infom227.small.analysis.{DummyObserver, SignAnalysis, SignAnalysisAbstractValue, Table, TableObserver, ZeroAnalysis, ZeroAnalysisAbstractValue, ZeroAnalysisInterpreter}
import be.unamur.info.infom227.small.ast.BuiltAstException
import be.unamur.info.infom227.small.cfg.ProgramPoint
import be.unamur.info.infom227.small.interpreter.VariableType
import org.antlr.v4.runtime.CharStreams

import scala.util.{Failure, Success, Try}

val SUCCESS_ERROR_CODE = 0
val COMPILATION_ERROR_CODE = 1
val FATAL_ERROR_CODE = 2
val UNKNOWN_ACTION_ERROR_CODE = 3

def parseArgument(string: String): Try[VariableType] =
  string.toIntOption
    .map(i => Success(i: VariableType))
    .orElse(string.toBooleanOption.map(b => Success(b: VariableType)))
    .getOrElse(Failure(new IllegalArgumentException(s"Cannot parse: $string")))

@main def main(action: String, file: String, others: String*): Unit = {
  action match {
    case "run" =>
      val tryResult = for {
        arguments <- others.foldRight(Try(List.empty[VariableType])) { (string, acc) =>
          for {
            args <- acc
            arg <- parseArgument(string)
          } yield arg :: args
        }
        charStream <- Try(CharStreams.fromFileName(file))
        programContext <- cst.parse(charStream)
        program <- ast.build(programContext)
        result <- interpreter.execute(program, "main", arguments)
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
        zeroAnalyses <- analysis.cfgsAnalysis(
          cfgs,
          (_, cfg) => Success(ZeroAnalysis(cfg)),
          (name, _) => Success(TableObserver[ZeroAnalysisAbstractValue](table = Table(bottomSymbol = ZeroAnalysisAbstractValue.Bottom.toString)))
        )
        moduleDiagnostics <- analysis.cfgsAnalysis(
          cfgs,
          (name, cfg) => zeroAnalyses.get(name) match {
            case Some((zeroAnalysis, _)) => Success(ZeroAnalysisInterpreter(cfg, zeroAnalysis))
            case None => Failure(new Exception(s"Zero analysis not found for $name"))
          },
          (_, _) => Success(DummyObserver())
        )
      } yield (zeroAnalyses, moduleDiagnostics)

      tryResult match {
        case Success((zeroAnalyses, moduleDiagnostics)) =>
          println("=====================================")
          println("            Zero Analysis            ")
          println("=====================================")
          for ((name, (_, observer)) <- zeroAnalyses) {
            println(s"Analysis for $name:")
            if (others.contains("-v")) {
              print(observer.table.toString)
            }

            moduleDiagnostics.get(name).foreach((diagnostics, _) =>
              for ((diagnosticType, message) <- diagnostics.abstractStates(ProgramPoint.ExitPoint).diagnostics) {
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
    case "sign-analysis" =>
      val tryResult = for {
        charStream <- Try(CharStreams.fromFileName(file))
        programContext <- cst.parse(charStream)
        program <- ast.build(programContext)
        cfgs = cfg.build(program)
        signAnalyses <- analysis.cfgsAnalysis(
          cfgs,
          (_, cfg) => Success(SignAnalysis(cfg)),
          (name, _) => Success(TableObserver[SignAnalysisAbstractValue](table = Table(bottomSymbol = SignAnalysisAbstractValue.Bottom.toString)))
        )
      } yield signAnalyses

      tryResult match {
        case Success(signAnalyses) =>
          println("=====================================")
          println("            Sign Analysis            ")
          println("=====================================")
          for ((name, (_, observer)) <- signAnalyses) {
            println(s"Analysis for $name:")
            if (others.contains("-v")) {
              print(observer.table.toString)
            }
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
