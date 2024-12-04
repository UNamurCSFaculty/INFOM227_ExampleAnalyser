package be.unamur.info.infom227.cfg

import be.unamur.info.infom227.ast.*


object ExampleCfgBuilder {
  def build(scopeAst: ExampleProgram | ExampleScope): ExampleCfg = {
    val (_, edges) = scopeAst.accept(ExampleCfgBuilder(), Set.empty)

    ExampleCfg(
      scopeAst match {
        case ExampleProgram(scope) => scope.variables
        case ExampleScope(variables, _*) => variables
      },
      edges.flatMap((startEdges, endProgramPoint) =>
        startEdges.map((startProgramPoint, condition) =>
          (startProgramPoint, endProgramPoint) -> condition
        )
      ).toMap
    )
  }
}

private type ExampleCfgStartEdges = Set[(ExampleProgramPoint, ExampleBooleanExpression)]
private type ExampleCfgEdges = Set[(ExampleCfgStartEdges, ExampleProgramPoint)]

private class ExampleCfgBuilder extends
  ExampleSequenceVisitor[(ExampleCfgStartEdges, ExampleCfgEdges), ExampleCfgStartEdges],
  ExampleStatementVisitor[(ExampleCfgStartEdges, ExampleCfgEdges), ExampleCfgStartEdges] {

  override def visitExampleProgram(node: ExampleProgram, startEdges: ExampleCfgStartEdges): (ExampleCfgStartEdges, ExampleCfgEdges) = {
    visitExampleScope(node.scope, startEdges)
  }

  private def visitSequence(statements: Seq[ExampleStatement], startEdges: ExampleCfgStartEdges): (ExampleCfgStartEdges, ExampleCfgEdges) = {
    statements.foldLeft[(ExampleCfgStartEdges, ExampleCfgEdges)]((startEdges, Set.empty)) {
      case ((startEdges, edges), statement) =>
        val (newStartEdges, newEdges) = statement.accept(this, startEdges)
        (
          newStartEdges,
          edges ++ newEdges
        )
    }
  }

  override def visitExampleScope(node: ExampleScope, startEdges: ExampleCfgStartEdges): (ExampleCfgStartEdges, ExampleCfgEdges) = {
    visitSequence(node.statements, startEdges)
  }

  override def visitExampleStatements(node: ExampleStatements, startEdges: ExampleCfgStartEdges): (ExampleCfgStartEdges, ExampleCfgEdges) = {
    visitSequence(node.statements, startEdges)
  }

  private def visitStatement(node: ExampleStatement, startEdges: ExampleCfgStartEdges): (ExampleCfgStartEdges, ExampleCfgEdges) = {
    val programPoint = ExampleProgramPoint(node)

    (
      Set((programPoint, ExampleBooleanConstant(true))),
      Set((startEdges, programPoint)),
    )
  }

  override def visitExampleDeclareStatement(node: ExampleDeclareStatement, startEdges: ExampleCfgStartEdges): (ExampleCfgStartEdges, ExampleCfgEdges) = {
    visitStatement(node, startEdges)
  }

  override def visitExampleAssignStatement(node: ExampleAssignStatement, startEdges: ExampleCfgStartEdges): (ExampleCfgStartEdges, ExampleCfgEdges) = {
    visitStatement(node, startEdges)
  }

  override def visitExamplePrintStatement(node: ExamplePrintStatement, startEdges: ExampleCfgStartEdges): (ExampleCfgStartEdges, ExampleCfgEdges) = {
    visitStatement(node, startEdges)
  }

  override def visitExampleIfStatement(node: ExampleIfStatement, startEdges: ExampleCfgStartEdges): (ExampleCfgStartEdges, ExampleCfgEdges) = {
    val programPoint = ExampleProgramPoint(node)

    val (ifStartEdges, ifEdges) = visitExampleStatements(node.ifStatements, Set((programPoint, node.condition)))
    val (elseStartEdges, elseEdges) = visitExampleStatements(node.elseStatements, Set((programPoint, node.condition.negate)))

    (
      ifStartEdges ++ elseStartEdges,
      Set((startEdges, programPoint)) ++ ifEdges ++ elseEdges
    )
  }

  override def visitExampleWhileStatement(node: ExampleWhileStatement, startEdges: ExampleCfgStartEdges): (ExampleCfgStartEdges, ExampleCfgEdges) = {
    val programPoint = ExampleProgramPoint(node)

    val (whileStartEdges, whileEdges) = visitExampleStatements(node.statements, Set((programPoint, node.condition)))

    (
      Set((programPoint, node.condition.negate)),
      Set((startEdges, programPoint), (whileStartEdges, programPoint)) ++ whileEdges
    )
  }
}
