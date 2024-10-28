package be.unamur.info.infom227

import be.unamur.info.infom227.ast.{CannotBuildAstException, ExampleAstBuilder}
import be.unamur.info.infom227.cst.ExampleParser
import be.unamur.info.infom227.interpreter.ExampleInterpreter
import com.sun.net.httpserver.Authenticator.Success
import org.antlr.v4.runtime.CharStreams

import scala.util.Try


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
    case action => println(f"Unknown action: $action")
  }
}
