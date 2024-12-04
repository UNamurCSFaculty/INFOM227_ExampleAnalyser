package be.unamur.info.infom227.analysis

import be.unamur.info.infom227.ast.{ExampleBooleanExpression, ExampleDeclareStatement}
import be.unamur.info.infom227.cfg.{ExampleCfg, ExampleProgramPoint}

import scala.util.Try


abstract class ExampleWorklist[L](lattice: ExampleLattice[L]) {
  def controlFlowFunction(p: ExampleProgramPoint, abstractEnvironment: ExampleAbstractEnvironment[String, L]): Try[ExampleAbstractEnvironment[String, L]]

  def conditionUpdateFunction(condition: ExampleBooleanExpression, abstractEnvironment: ExampleAbstractEnvironment[String, L]): Try[Map[String, L]]

  private def getVariables(cfg: ExampleCfg, programPoint: ExampleProgramPoint): Set[String] = {
    programPoint.statement match
      case ExampleDeclareStatement(_, name, _) => Set(name) ++ cfg.succ(programPoint).flatMap(getVariables(cfg, _))
      case _ => Set.empty
  }

  def worklist(cfg: ExampleCfg): Try[Map[ExampleProgramPoint, ExampleAbstractEnvironment[String, L]]] = {
    Try {
      val entryPoint = cfg.entryPoint.get
      val bottom = lattice.bottom.get
      val variables = getVariables(cfg, entryPoint)

      val abstractEnvironments = cfg.programPoints.map(
        _ -> ExampleAbstractEnvironment(lattice, None, variables.map(_ -> bottom).toMap)
      ).to(scala.collection.mutable.Map)

      val WL = scala.collection.mutable.Set(entryPoint)

      while (WL.nonEmpty) {
        val p = WL.head
        WL.remove(p)

        val res = controlFlowFunction(p, abstractEnvironments(p)).get

        for (r <- cfg.succ(p)) {
          val cond = cfg.cond(p, r).get

          val resCond = res.set(conditionUpdateFunction(cond, res).get)

          if (!abstractEnvironments(r).includes(resCond)) {
            abstractEnvironments(r) = abstractEnvironments(r).join(resCond).get

            WL.add(r)
          }
        }
      }

      abstractEnvironments.toMap
    }
  }
}
