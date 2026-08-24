package be.unamur.info.infom227.cfg

import be.unamur.info.infom227.ast.BooleanExpression

case class Cfg(edges: Map[(ProgramPoint, ProgramPoint), BooleanExpression]) {

  def successors(programPoint: ProgramPoint): Set[ProgramPoint] = {
    edges.keys.filter(_._1 == programPoint).map(_._2).toSet
  }

  def predecessors(programPoint: ProgramPoint): Set[ProgramPoint] = {
    edges.keys.filter(_._2 == programPoint).map(_._1).toSet
  }

  def condition(start: ProgramPoint, end: ProgramPoint): Option[BooleanExpression] = {
    edges.get((start, end))
  }

  def entryPoints: Set[ProgramPoint] = {
    edges.keys.map(_._1).filter(predecessors(_).isEmpty).toSet
  }

  def programPoints: Set[ProgramPoint] = {
    edges.keys.flatMap((startProgramPoint, endProgramPoint) => Set(startProgramPoint, endProgramPoint)).toSet
  }

  def dot(name: String): String = {
    val builder = new StringBuilder
    builder.append(s"digraph \"$name\" {\n")
    for (((start, end), condition) <- edges) {
      builder.append(s"""    "$start" -> "$end" [label="$condition"];\n""")
    }
    builder.append("}\n")
    builder.toString()
  }
}
