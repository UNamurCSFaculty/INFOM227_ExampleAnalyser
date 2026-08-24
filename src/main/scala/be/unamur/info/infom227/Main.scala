package be.unamur.info.infom227

import be.unamur.info.infom227.ast.BuiltAstException
import org.antlr.v4.runtime.CharStreams

import scala.util.{Failure, Success, Try}

val SUCCESS_ERROR_CODE = 0
val COMPILATION_ERROR_CODE = 1
val FATAL_ERROR_CODE = 2
val UNKNOWN_ACTION_ERROR_CODE = 3

@main def main(action: String, file: String): Unit = {
  action match {
    case "run" =>
      val tryResult = for {
        charStream <- Try(CharStreams.fromFileName(file))
        programContext <- cst.parse(charStream)
        program <- ast.build(programContext)
        result <- interpreter.execute(program)
      } yield result

      tryResult match
        case Success(messages) =>
          System.exit(SUCCESS_ERROR_CODE)
        case Failure(exception: BuiltAstException) =>
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
