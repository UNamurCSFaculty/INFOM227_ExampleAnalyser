package be.unamur.info.infom227.small.ast

import be.unamur.info.infom227.small.cst.{SmallGrammarBaseVisitor, SmallGrammarParser}

import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

case class BuiltAstException(message: String) extends Exception(message)

def build(programCst: SmallGrammarParser.ProgramContext): Try[Program] = {
  Builder.visitProgram(programCst)
}

type VisitorType = Try[Program] | Function | List[String] | List[Statement] | Statement | List[Expression] | FunctionCall | Expression | ArithmeticBinaryOperator | IntegerComparisonOperator | EqualComparisonOperator | BooleanBinaryOperator

private object Builder extends SmallGrammarBaseVisitor[VisitorType] {
  // Program
  override def visitProgram(ctx: SmallGrammarParser.ProgramContext): Try[Program] = {
    for {
      functions <- ctx
        .function
        .asScala.foldLeft(Success(Map.empty): Try[Map[String, Function]]) { (acc, functionCtx) =>
          for {
            functions <- acc
            functionName = functionCtx.IDENTIFIER.getText
            newFunctions <- if (functions.contains(functionName)) {
              Failure(BuiltAstException(s"Duplicate function definition: ${functionName}"))
            } else {
              Success(functions + (functionName -> visitFunction(functionCtx)))
            }
          } yield newFunctions
        }
    } yield Program(functions)
  }

  override def visitFunction(ctx: SmallGrammarParser.FunctionContext): Function = {
    Function(ctx.start.getLine, visitParameters(ctx.parameters), visitBody(ctx.body))
  }

  override def visitParameters(ctx: SmallGrammarParser.ParametersContext): List[String] = {
    ctx
      .IDENTIFIER
      .asScala
      .iterator
      .map(_.getText)
      .toList
  }

  override def visitBody(ctx: SmallGrammarParser.BodyContext): List[Statement] = {
    ctx
      .stmt
      .asScala
      .iterator
      .map(visitStmt)
      .toList
  }

  // Statements
  override def visitStmt(ctx: SmallGrammarParser.StmtContext): Statement = {
    if (ctx.assignStmt != null) {
      visitAssignStmt(ctx.assignStmt)
    } else if (ctx.ifStmt != null) {
      visitIfStmt(ctx.ifStmt)
    } else if (ctx.whileStmt != null) {
      visitWhileStmt(ctx.whileStmt)
    } else if (ctx.returnStmt != null) {
      visitReturnStmt(ctx.returnStmt)
    } else {
      throw new AssertionError(s"Unsupported statement : ${ctx.getText}")
    }
  }

  override def visitAssignStmt(ctx: SmallGrammarParser.AssignStmtContext): AssignStatement = {
    if (ctx.expr != null) {
      AssignStatement(ctx.start.getLine, ctx.IDENTIFIER.getText, visitExpr(ctx.expr))
    } else if (ctx.funcCall != null) {
      AssignStatement(ctx.start.getLine, ctx.IDENTIFIER.getText, visitFuncCall(ctx.funcCall))
    } else {
      throw new AssertionError(s"Unsupported assignment statement : ${ctx.getText}")
    }
  }

  override def visitIfStmt(ctx: SmallGrammarParser.IfStmtContext): IfStatement = {
    IfStatement(ctx.start.getLine, visitBoolExpr(ctx.boolExpr), visitBody(ctx.ifBody), visitBody(ctx.elseBody))
  }

  override def visitWhileStmt(ctx: SmallGrammarParser.WhileStmtContext): WhileStatement = {
    WhileStatement(ctx.start.getLine, visitBoolExpr(ctx.boolExpr), visitBody(ctx.body))
  }

  override def visitReturnStmt(ctx: SmallGrammarParser.ReturnStmtContext): ReturnStatement = {
    ReturnStatement(ctx.start.getLine, visitExpr(ctx.expr))
  }

  // Call
  override def visitArguments(ctx: SmallGrammarParser.ArgumentsContext): List[Expression] = {
    ctx.expr().asScala.map(visitExpr).toList
  }

  override def visitFuncCall(ctx: SmallGrammarParser.FuncCallContext): FunctionCall = {
    FunctionCall(ctx.IDENTIFIER.getText, visitArguments(ctx.arguments()))
  }

  // Expressions
  override def visitExpr(ctx: SmallGrammarParser.ExprContext): Expression = {
    if (ctx.arithExpr != null) {
      visitArithExpr(ctx.arithExpr)
    } else if (ctx.boolExpr != null) {
      visitBoolExpr(ctx.boolExpr)
    } else {
      throw new AssertionError(s"Unsupported expression : ${ctx.getText}")
    }
  }

  override def visitArithExpr(ctx: SmallGrammarParser.ArithExprContext): ArithmeticExpression = {
    if (ctx.noprnd != null) {
      visitNoprnd(ctx.noprnd)
    } else if (ctx.binArithOp != null) {
      visitBinArithOp(ctx.binArithOp)
    } else {
      throw new AssertionError(s"Unsupported arithmetic expression : ${ctx.getText}")
    }
  }

  override def visitBoolExpr(ctx: SmallGrammarParser.BoolExprContext): BooleanExpression = {
    if (ctx.boprnd != null) {
      visitBoprnd(ctx.boprnd)
    } else if (ctx.relOp != null) {
      visitRelOp(ctx.relOp)
    } else if (ctx.binLogicOp != null) {
      visitBinLogicOp(ctx.binLogicOp)
    } else {
      throw new AssertionError(s"Unsupported boolean expression : ${ctx.getText}")
    }
  }

  override def visitBinArithOp(ctx: SmallGrammarParser.BinArithOpContext): ArithmeticExpression = {
    ArithmeticBinaryOperation(visitNoprnd(ctx.left), visitArithOp(ctx.arithOp), visitNoprnd(ctx.right))
  }

  override def visitBinLogicOp(ctx: SmallGrammarParser.BinLogicOpContext): BooleanExpression = {
    IntegerComparisonOperation(visitNoprnd(ctx.left), visitLogicOp(ctx.logicOp), visitNoprnd(ctx.right))
  }

  override def visitRelOp(ctx: SmallGrammarParser.RelOpContext): BooleanExpression = {
    BooleanBinaryOperation(visitBoprnd(ctx.left), visitNop(ctx.nop), visitBoprnd(ctx.right))
  }

  override def visitNoprnd(ctx: SmallGrammarParser.NoprndContext): ArithmeticOperand = {
    if (ctx.IDENTIFIER != null) {
      Variable(ctx.IDENTIFIER.getText)
    } else if (ctx.NUM != null) {
      ArithmeticConstant(ctx.NUM.getText.toInt)
    } else {
      throw new AssertionError(s"Unsupported arithmetic operand : ${ctx.getText}")
    }
  }

  override def visitBoprnd(ctx: SmallGrammarParser.BoprndContext): BooleanOperand = {
    if (ctx.IDENTIFIER != null) {
      Variable(ctx.IDENTIFIER.getText)
    } else if (ctx.TRUE != null) {
      BooleanConstant(true)
    } else if (ctx.FALSE != null) {
      BooleanConstant(false)
    } else {
      throw new AssertionError(s"Unsupported boolean operand : ${ctx.getText}")
    }
  }

  // Operators
  override def visitArithOp(ctx: SmallGrammarParser.ArithOpContext): ArithmeticBinaryOperator = {
    if (ctx.ADD != null) {
      ArithmeticBinaryOperator.Add
    } else if (ctx.SUBSTRACT != null) {
      ArithmeticBinaryOperator.Sub
    } else if (ctx.MULTIPLY != null) {
      ArithmeticBinaryOperator.Mul
    } else if (ctx.DIVIDE != null) {
      ArithmeticBinaryOperator.Div
    } else {
      throw new AssertionError(s"Unsupported arithmetic operator : ${ctx.getText}")
    }
  }

  override def visitLogicOp(ctx: SmallGrammarParser.LogicOpContext): IntegerComparisonOperator | EqualComparisonOperator = {
    if (ctx.LESS != null) {
      IntegerComparisonOperator.Lt
    } else if (ctx.GREATER != null) {
      IntegerComparisonOperator.Gt
    } else if (ctx.EQUAL != null) {
      EqualComparisonOperator.Eq
    } else if (ctx.NOT_EQUAL != null) {
      EqualComparisonOperator.Ne
    } else if (ctx.LESS_EQUAL != null) {
      IntegerComparisonOperator.Lte
    } else if (ctx.GREATER_EQUAL != null) {
      IntegerComparisonOperator.Gte
    } else {
      throw new AssertionError(s"Unsupported logic operator : ${ctx.getText}")
    }
  }

  override def visitNop(ctx: SmallGrammarParser.NopContext): BooleanBinaryOperator | EqualComparisonOperator = {
    if (ctx.EQUAL != null) {
      EqualComparisonOperator.Eq
    } else if (ctx.NOT_EQUAL != null) {
      EqualComparisonOperator.Ne
    } else if (ctx.AND != null) {
      BooleanBinaryOperator.And
    } else if (ctx.OR != null) {
      BooleanBinaryOperator.Or
    } else {
      throw new AssertionError(s"Unsupported comparison operator : ${ctx.getText}")
    }
  }
}
