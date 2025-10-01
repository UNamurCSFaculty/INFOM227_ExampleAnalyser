package be.unamur.info.infom227

import be.unamur.info.infom227.analysis.{ExampleZeroAnalysisResultsInterpreter, ExampleZeroAnalysisWorklist}
import be.unamur.info.infom227.ast.{CannotBuildAstException, ExampleAstBuilder}
import be.unamur.info.infom227.cfg.ExampleCfgBuilder
import be.unamur.info.infom227.cst.ExampleParser
import be.unamur.info.infom227.interpreter.ExampleInterpreter
import org.antlr.v4.runtime.CharStreams

import scala.util.{Try, Success, Failure}

val SUCCESS_ERROR_CODE = 0
val COMPILATION_ERROR_CODE = 1
val FATAL_ERROR_CODE = 2
val UNKNOWN_ACTION_ERROR_CODE = 3

@main def main(action: String, file: String): Unit = {
  action match {
    case "run" =>
      val tryResult = for {
        charStream <- Try(CharStreams.fromFileName(file))
        cst <- ExampleParser.parse(charStream)
        ast <- ExampleAstBuilder.build(cst)
        result <- ExampleInterpreter.run(ast)
      } yield result

      tryResult match
        case Success(messages) =>
          System.exit(SUCCESS_ERROR_CODE)
        case Failure(exception: CannotBuildAstException) =>
          println(s"Compilation Error:\n${exception.getMessage}")
          System.exit(COMPILATION_ERROR_CODE)
        case Failure(exception: Throwable) =>
          println(s"Fatal error:\n${exception.getMessage}")
          System.exit(FATAL_ERROR_CODE)
    case "zero-analysis" =>
      val tryMessages = for {
        charStream <- Try(CharStreams.fromFileName(file))
        cst <- ExampleParser.parse(charStream)
        ast <- ExampleAstBuilder.build(cst)
        cfg = ExampleCfgBuilder.build(ast)
        results <- ExampleZeroAnalysisWorklist.worklist(cfg)
        messages <- ExampleZeroAnalysisResultsInterpreter.interpret(cfg, results)
      } yield messages

      tryMessages match
        case Success(messages) =>
          for ((lineNumber, messageType, message) <- messages) {
            println(s"[$messageType] Line $lineNumber : $message")
          }
          System.exit(SUCCESS_ERROR_CODE)
        case Failure(exception: CannotBuildAstException) =>
          println(s"Compilation Error:\n${exception.getMessage}")
          System.exit(COMPILATION_ERROR_CODE)
        case Failure(exception: Throwable) =>
          println(s"Fatal error:\n${exception.getMessage}")
          System.exit(FATAL_ERROR_CODE)
    case action =>
      println(f"Unknown action: $action")
      System.exit(UNKNOWN_ACTION_ERROR_CODE)
  }
}
