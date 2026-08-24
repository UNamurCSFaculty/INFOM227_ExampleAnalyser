package be.unamur.info.infom227.cfg

import be.unamur.info.infom227.ast.Statement

enum ProgramPoint {
  case EntryPoint
  case StatementPoint(statement: Statement)
  case ExitPoint

  override def toString: String = this match {
    case EntryPoint => "EntryPoint"
    case StatementPoint(statement) => s"PP(${statement.lineNumber.toString})"
    case ExitPoint => "ExitPoint"
  }
}
