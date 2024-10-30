package be.unamur.info.infom227

import be.unamur.info.infom227.analysis.{ExampleZeroAnalysisResultsInterpreter, ExampleZeroAnalysisWorklist}
import be.unamur.info.infom227.ast.{CannotBuildAstException, ExampleAstBuilder}
import be.unamur.info.infom227.cfg.ExampleCfgBuilder
import be.unamur.info.infom227.cst.ExampleParser
import be.unamur.info.infom227.interpreter.ExampleInterpreter
import org.antlr.v4.runtime.CharStreams

import scala.util.{Try, Success, Failure}


@main def main(action: String, file: String): Unit = {
  action match {
    case "run" =>
      val tryResult = for {
        charStream <- Try(CharStreams.fromFileName(file))
        cst <- ExampleParser.parse(charStream)
        ast <- ExampleAstBuilder.build(cst)
        result <- ExampleInterpreter.run(ast)
      } yield result

      tryResult.recover {
        case exception: CannotBuildAstException => println(s"Compilation Error:\n${exception.getMessage}")
        case exception: Throwable => println(s"Fatal error:\n${exception.getMessage}")
      }
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
        case Failure(exception: CannotBuildAstException) => println(s"Compilation Error:\n${exception.getMessage}")
        case Failure(exception: Throwable) => println(s"Fatal error:\n${exception.getMessage}")
    case action => println(f"Unknown action: $action")
  }
}
