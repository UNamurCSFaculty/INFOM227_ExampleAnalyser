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

object ProgramPoint:
  given Ordering[ProgramPoint] =
    Ordering.by {
      case ProgramPoint.EntryPoint                => (0, 0)
      case ProgramPoint.StatementPoint(statement) => (1, statement.lineNumber)
      case ProgramPoint.ExitPoint                 => (2, 0)
    }
