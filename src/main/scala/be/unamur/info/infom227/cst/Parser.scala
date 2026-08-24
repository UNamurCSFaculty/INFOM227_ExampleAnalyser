package be.unamur.info.infom227.cst

import org.antlr.v4.runtime.{CharStream, CommonTokenStream, ConsoleErrorListener, RecognitionException, Recognizer}

import scala.util.{Failure, Success, Try}

case class ParseException(message: String) extends Exception(message)

def parse(stream: CharStream): Try[SmallGrammarParser.ProgramContext] = {
  val lexer = SmallGrammarLexer(stream)
  val tokens = CommonTokenStream(lexer)
  val parser = SmallGrammarParser(tokens)

  parser.removeErrorListeners()
  var errors: List[String] = List.empty
  parser.addErrorListener(new ConsoleErrorListener {
    override def syntaxError(recognizer: Recognizer[?, ?], offendingSymbol: AnyRef, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
      errors = f"line $line:$charPositionInLine $msg" :: errors
    }
  })

  for {
    cst <- Try(parser.program())
    _ <- if (errors.nonEmpty) {
      Failure(ParseException(errors.mkString("\n")))
    } else {
      Success(())
    }
  } yield cst
}
