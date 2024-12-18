package be.unamur.info.infom227.ast

import org.antlr.v4.runtime.ParserRuleContext

class CannotBuildAstException(
                               private val message: String,
                               private val line: Integer,
                               private val cause: Throwable
                             ) extends Exception(if (line != null) s"Line $line - $message" else message, cause) {
  def this(message: String = "", context: ParserRuleContext = None.orNull, cause: Throwable = None.orNull) =
    this(message, context.getStart.getLine, cause)
}

class UnsupportedRuleException(
                                private val message: String,
                                private val context: ParserRuleContext,
                                private val cause: Throwable
                              ) extends CannotBuildAstException(message, context, cause)

class MissingContextException(
                               private val message: String
                             ) extends CannotBuildAstException(message)
