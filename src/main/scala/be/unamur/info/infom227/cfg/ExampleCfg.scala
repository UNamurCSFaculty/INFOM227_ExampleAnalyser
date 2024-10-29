package be.unamur.info.infom227.cfg

import be.unamur.info.infom227.ast.ExampleBooleanExpression

case class ExampleCfg(edges: Map[(ExampleProgramPoint, ExampleProgramPoint), ExampleBooleanExpression]) {

  def succ(programPoint: ExampleProgramPoint): Set[ExampleProgramPoint] = {
    edges.keys.filter(_._1 == programPoint).map(_._2).toSet
  }

  def pred(programPoint: ExampleProgramPoint): Set[ExampleProgramPoint] = {
    edges.keys.filter(_._2 == programPoint).map(_._1).toSet
  }

  def cond(startProgramPoint: ExampleProgramPoint, endProgramPoint: ExampleProgramPoint): Option[ExampleBooleanExpression] = {
    edges.get((startProgramPoint, endProgramPoint))
  }

  def entryPoint: Option[ExampleProgramPoint] = {
    edges.keys.map(_._1).find(pred(_).isEmpty)
  }
}
