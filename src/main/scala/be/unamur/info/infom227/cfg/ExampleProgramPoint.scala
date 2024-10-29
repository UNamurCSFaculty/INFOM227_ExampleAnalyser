package be.unamur.info.infom227.cfg

import be.unamur.info.infom227.ast.ExampleStatement

case class ExampleProgramPoint(statement: ExampleStatement) {
  override def toString: String = s"PP(${statement.lineNumber.toString})"
}
