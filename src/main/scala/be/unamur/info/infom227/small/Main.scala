package be.unamur.info.infom227.small

import be.unamur.info.infom227.small.analysis.*
import be.unamur.info.infom227.small.ast.{BuiltAstException, Program}
import be.unamur.info.infom227.small.cfg.ProgramPoint
import be.unamur.info.infom227.small.interpreter.VariableType
import org.antlr.v4.runtime.CharStreams

import scala.util.{Failure, Success, Try}

val SUCCESS_ERROR_CODE = 0
val COMPILATION_ERROR_CODE = 1
val FATAL_ERROR_CODE = 2
val UNKNOWN_ACTION_ERROR_CODE = 3

def parseArgument(string: String): Try[VariableType] = {
  string.toIntOption
    .map(i => Success(i: VariableType))
    .orElse(string.toBooleanOption.map(b => Success(b: VariableType)))
    .getOrElse(Failure(new IllegalArgumentException(s"Cannot parse: $string")))
}

def buildAstFromFileName(filename: String): Try[Program] = {
  for {
    charStream <- Try(CharStreams.fromFileName(filename))
    programContext <- cst.parse(charStream)
    program <- ast.build(programContext)
  } yield program
}

def printTitle(title: String, size: Int = 50): Unit = {
  println("=" * size)
  println(" " * ((size - title.length) / 2) ++ title)
  println("=" * size)
}

def printAnalysis[T <: Lattice[T], S <: State[T]](
  analyses: Map[String, (AnalysisState[S], TableObserver[T, S])],
  verbose: Boolean,
  f: String => Unit = _ => {}
): Unit = {
  for ((name, (_, observer)) <- analyses) {
    println(s"Analysis for $name:")
    if (verbose) {
      print(observer.table.toString)
    }
    f(name)
  }
}

def handleExit[T](tryResult: Try[T], f: T => Unit): Unit = {
  tryResult match
    case Success(value) =>
      Try(f(value)) match {
        case Success(value) =>
          System.exit(SUCCESS_ERROR_CODE)
        case Failure(exception) =>
          println(s"Fatal error:\n${exception.getMessage}")
          exception.printStackTrace()
          System.exit(FATAL_ERROR_CODE)
      }
    case Failure(exception: BuiltAstException) =>
      println(s"Compilation Error:\n${exception.getMessage}")
      System.exit(COMPILATION_ERROR_CODE)
    case Failure(exception: Throwable) =>
      println(s"Fatal error:\n${exception.getMessage}")
      exception.printStackTrace()
      System.exit(FATAL_ERROR_CODE)
}

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
        program <- buildAstFromFileName(file)
        result <- interpreter.execute(program, "main", arguments)
      } yield result

      handleExit(
        tryResult,
        returnValue => {
          println(returnValue)
        }
      )
    case "zero-analysis" =>
      val tryResult = for {
        program <- buildAstFromFileName(file)
        cfgs = cfg.build(program)
        zeroAnalyses <- cfgsAnalysis(
          cfgs,
          (_, cfg) => Success(ZeroAnalysis(cfg)),
          (name, _) => Success(TableObserver(table = Table(bottomSymbol = ZeroAnalysisAbstractValue.Bottom.toString)))
        )
        moduleDiagnostics <- cfgsAnalysis(
          cfgs,
          (name, cfg) => {
            zeroAnalyses.get(name) match {
              case Some((zeroAnalysis, _)) => Success(ZeroAnalysisInterpreter(cfg, zeroAnalysis))
              case None => Failure(new Exception(s"Zero analysis not found for $name"))
            }
          },
          (_, _) => Success(DummyObserver())
        )
      } yield (zeroAnalyses, moduleDiagnostics)

      handleExit(
        tryResult,
        (zeroAnalyses, moduleDiagnostics) => {
          printTitle("Zero Analysis")
          printAnalysis(
            zeroAnalyses,
            others.contains("-v"),
            name => {
              moduleDiagnostics.get(name).foreach((diagnostics, _) => {
                for ((diagnosticType, message) <- diagnostics.abstractStates(ProgramPoint.ExitPoint).diagnostics) {
                  println(s"  [$diagnosticType] $message\n")
                }
              })
            }
          )
        }
      )
    case "sign-analysis" =>
      val tryResult = for {
        program <- buildAstFromFileName(file)
        cfgs = cfg.build(program)
        signAnalyses <- cfgsAnalysis(
          cfgs,
          (_, cfg) => Success(SignAnalysis(cfg)),
          (name, _) => Success(TableObserver(table = Table(bottomSymbol = SignAnalysisAbstractValue.Bottom.toString)))
        )
      } yield signAnalyses

      handleExit(
        tryResult,
        signAnalyses => {
          printTitle("Sign Analysis")
          printAnalysis(signAnalyses, others.contains("-v"))
        }
      )
    case "constant-analysis" =>
      val tryResult = for {
        program <- buildAstFromFileName(file)
        cfgs = cfg.build(program)
        constantAnalyses <- cfgsAnalysis(
          cfgs,
          (_, cfg) => Success(ConstantAnalysis(cfg)),
          (name, _) => Success(TableObserver(table = Table(bottomSymbol = ConstantAnalysisAbstractValue.Bottom.toString)))
        )
      } yield constantAnalyses

      handleExit(
        tryResult,
        constantAnalyses => {
          printTitle("Constant Analysis")
          printAnalysis(constantAnalyses, others.contains("-v"))
        }
      )
    case "interprocedural-constant-analysis" =>
      val tryResult = for {
        program <- buildAstFromFileName(file)
        cfgs = cfg.build(program)
        interproceduralObserver = InterproceduralTableObserver[ConstantAnalysisAbstractValue, TableObserver[ConstantAnalysisAbstractValue, ExtendedConstantAnalysisAbstractState], InterproceduralAbstractState[ExtendedConstantAnalysisAbstractState, TableObserver[ConstantAnalysisAbstractValue, ExtendedConstantAnalysisAbstractState]]]()
        constantAnalyses <- interproceduralAnalysis(
          InterproceduralConstantCallGraphAnalysis(
            cfgs,
            (_, _) => Success(TableObserver(table = Table(bottomSymbol = ConstantAnalysisAbstractValue.Bottom.toString)))
          ),
          interproceduralObserver
        )
      } yield (constantAnalyses, interproceduralObserver)

      handleExit(
        tryResult,
        (constantAnalyses, interproceduralObserver) => {
          printTitle("Interprocedural Constant Analysis")
          for ((name, observer, analysisState, worklist) <- interproceduralObserver.trace) {
            println(s"Analysis for $name:")
            if (others.contains("-v")) {
              print(observer.table.toString)
              for ((name, functionState) <- analysisState.functions) {
                println(s"> function $name(${functionState.arguments.map(_.toString).mkString(", ")}) -> ${functionState.returnValue}")
                println(s"      Callees: {${functionState.callees.map(_.toString).mkString(", ")}}")
              }
              println(s"> Worklist: {${worklist.mkString(", ")}}")
            }
          }
          println("Signatures:")
          for ((name, functionState) <- constantAnalyses.functions) {
            println(s"    ${name}(${functionState.arguments.map(argument => argument.toString).mkString(", ")}) -> ${functionState.returnValue}")
          }
        }
      )
    case "interval-analysis" =>
      val tryResult = for {
        program <- buildAstFromFileName(file)
        cfgs = cfg.build(program)
        intervalAnalyses <- cfgsAnalysis(
          cfgs,
          (_, cfg) => Success(IntervalAnalysis(cfg)),
          (name, _) => Success(TableObserver(table = Table(bottomSymbol = IntervalAnalysisAbstractValue.Bottom.toString)))
        )
      } yield intervalAnalyses

      handleExit(
        tryResult,
        intervalAnalyses => {
          printTitle("Interval Analysis")
          printAnalysis(intervalAnalyses, others.contains("-v"))
        }
      )
    case action =>
      println(f"Unknown action: $action")
      System.exit(UNKNOWN_ACTION_ERROR_CODE)
  }
}
