package be.unamur.info.infom227.cfg

import be.unamur.info.infom227.ast.*
import be.unamur.info.infom227.cfg.ProgramPoint.StatementPoint

import scala.util.control.Breaks._
import scala.collection.mutable

private type PreviousPoints = Set[(ProgramPoint, BooleanExpression)]

private case class ResultPoints(previousPoints: PreviousPoints, returnPoints: PreviousPoints)

private type Edges = mutable.Map[(ProgramPoint, ProgramPoint), BooleanExpression]

def build(program: Program): Map[String, Cfg] = {
  program.functions.map((name, function) => name -> analyseFunction(function)).toMap
}

private def createProgramPoint(edges: Edges, previousPoints: PreviousPoints, programPoint: ProgramPoint): ProgramPoint = {
  for ((previousPoint, edge) <- previousPoints) {
    edges += (previousPoint, programPoint) -> edge
  }

  programPoint
}

private def analyseFunction(node: Function): Cfg = {
  val edges: Edges = mutable.Map()

  val resultPoints = analyseBody(edges, Set(ProgramPoint.EntryPoint -> BooleanConstant(true)), node.body)

  createProgramPoint(edges, resultPoints.previousPoints, ProgramPoint.ExitPoint)
  createProgramPoint(edges, resultPoints.returnPoints, ProgramPoint.ExitPoint)

  Cfg(edges.toMap)
}

private def analyseBody(edges: Edges, previousPoints: PreviousPoints, body: List[Statement]): ResultPoints = {
  var currentPreviousEdges = previousPoints
  var returnPoints: PreviousPoints = Set.empty

  breakable {
    for (statement <- body) {
      val stmtResultPoints = statement match {
        case assignStatement: AssignStatement => analyseAssignStatement(edges, currentPreviousEdges, assignStatement)
        case ifStatement: IfStatement => analyseIfStatement(edges, currentPreviousEdges, ifStatement)
        case whileStatement: WhileStatement => analyseWhileStatement(edges, currentPreviousEdges, whileStatement)
        case returnStatement: ReturnStatement => analyseReturnStatement(edges, currentPreviousEdges, returnStatement)
      }

      currentPreviousEdges = stmtResultPoints.previousPoints
      returnPoints ++= stmtResultPoints.returnPoints

      if (stmtResultPoints.previousPoints.isEmpty && stmtResultPoints.returnPoints.nonEmpty) {
        break()
      }
    }
  }

  ResultPoints(currentPreviousEdges, returnPoints)
}

private def analyseAssignStatement(edges: Edges, previousEdges: PreviousPoints, assignStatement: AssignStatement): ResultPoints = {
  val programPoint = createProgramPoint(edges, previousEdges, StatementPoint(assignStatement))

  ResultPoints(Set((programPoint, BooleanConstant(true))), Set.empty)
}

private def analyseIfStatement(edges: Edges, previousEdges: PreviousPoints, ifStatement: IfStatement): ResultPoints = {
  val programPoint = createProgramPoint(edges, previousEdges, StatementPoint(ifStatement))

  val ifResultPoints = analyseBody(edges, Set((programPoint, ifStatement.condition)), ifStatement.ifBody)
  val elseResultPoints = analyseBody(edges, Set((programPoint, ifStatement.condition.negate)), ifStatement.elseBody)

  ResultPoints(ifResultPoints.previousPoints ++ elseResultPoints.previousPoints, ifResultPoints.returnPoints ++ elseResultPoints.returnPoints)
}

private def analyseWhileStatement(edges: Edges, previousEdges: PreviousPoints, whileStatement: WhileStatement): ResultPoints = {
  val programPoint = createProgramPoint(edges, previousEdges, StatementPoint(whileStatement))

  val resultPoints = analyseBody(edges, Set((programPoint, whileStatement.condition)), whileStatement.body)

  for ((previousPoint, edge) <- resultPoints.previousPoints) {
    edges += (previousPoint, programPoint) -> edge
  }

  ResultPoints(Set((programPoint, whileStatement.condition.negate)), resultPoints.returnPoints)
}

private def analyseReturnStatement(edges: Edges, previousEdges: PreviousPoints, returnStatement: ReturnStatement): ResultPoints = {
  val programPoint = createProgramPoint(edges, previousEdges, StatementPoint(returnStatement))

  ResultPoints(Set.empty, Set((programPoint, BooleanConstant(true))))
}
