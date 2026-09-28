package be.unamur.info.infom227.small.cfg

import be.unamur.info.infom227.small.ast.Statement

enum ProgramPoint {
  case EntryPoint
  case StatementPoint(statement: Statement)
  case ExitPoint

  override def toString: String = this match {
    case EntryPoint => "○"
    case StatementPoint(statement) => s"${statement.lineNumber.toString}"
    case ExitPoint => "◎"
  }
}
