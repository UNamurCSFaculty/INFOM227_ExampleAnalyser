package be.unamur.info.infom227.cfg

import be.unamur.info.infom227.ast.*
import org.scalatest.Assertions.fail


object FromLineNumber {
  def pp(program: ExampleProgram, lineNumber: Int): ExampleProgramPoint = {
    FromLineNumber().visitExampleProgram(program, lineNumber) match
      case Some(statement) => ExampleProgramPoint(statement)
      case None => fail(s"Unknown program line: $lineNumber")
  }
}


private class FromLineNumber extends ExampleSequenceVisitor[Option[ExampleStatement], Int], ExampleStatementVisitor[Option[ExampleStatement], Int] {

  override def visitExampleProgram(node: ExampleProgram, lineNumber: Int): Option[ExampleStatement] = {
    visitExampleScope(node.scope, lineNumber)
  }

  private def visitSequence(statements: Seq[ExampleStatement], lineNumber: Int): Option[ExampleStatement] = {
    statements.foldLeft[Option[ExampleStatement]](None) {
      (acc, statement) => acc.orElse(statement.accept(this, lineNumber))
    }
  }

  override def visitExampleScope(node: ExampleScope, lineNumber: Int): Option[ExampleStatement] = {
    visitSequence(node.statements, lineNumber)
  }

  override def visitExampleStatements(node: ExampleStatements, lineNumber: Int): Option[ExampleStatement] = {
    visitSequence(node.statements, lineNumber)
  }

  private def visitStatement(node: ExampleStatement, lineNumber: Int): Option[ExampleStatement] = {
    if (node.lineNumber == lineNumber) {
      Some(node)
    } else {
      None
    }
  }

  override def visitExampleDeclareStatement(node: ExampleDeclareStatement, lineNumber: Int): Option[ExampleStatement] = {
    visitStatement(node, lineNumber)
  }

  override def visitExampleAssignStatement(node: ExampleAssignStatement, lineNumber: Int): Option[ExampleStatement] = {
    visitStatement(node, lineNumber)
  }

  override def visitExamplePrintStatement(node: ExamplePrintStatement, lineNumber: Int): Option[ExampleStatement] = {
    visitStatement(node, lineNumber)
  }

  override def visitExampleIfStatement(node: ExampleIfStatement, lineNumber: Int): Option[ExampleStatement] = {
    visitStatement(node, lineNumber)
      .orElse(visitSequence(node.ifStatements.statements, lineNumber))
      .orElse(visitSequence(node.elseStatements.statements, lineNumber))
  }

  override def visitExampleWhileStatement(node: ExampleWhileStatement, lineNumber: Int): Option[ExampleStatement] = {
    visitStatement(node, lineNumber).orElse(visitSequence(node.statements.statements, lineNumber))
  }
}
