package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast.*
import be.unamur.info.infom227.small.cfg.{Cfg, ProgramPoint}

import scala.util.Try

enum IntervalAnalysisAbstractValue extends Lattice[IntervalAnalysisAbstractValue]:
  case Unknown
  case Bottom

  override def join(other: IntervalAnalysisAbstractValue): IntervalAnalysisAbstractValue = {
    (this, other) match {
      case (IntervalAnalysisAbstractValue.Bottom, _) => other
      case (_, IntervalAnalysisAbstractValue.Bottom) => this
      case _ => IntervalAnalysisAbstractValue.Unknown
    }
  }

  override def meet(other: IntervalAnalysisAbstractValue): IntervalAnalysisAbstractValue = {
    (this, other) match {
      case (IntervalAnalysisAbstractValue.Unknown, _) => other
      case (_, IntervalAnalysisAbstractValue.Unknown) => this
      case _ => IntervalAnalysisAbstractValue.Bottom
    }
  }

  override def toString: String = {
    this match {
      case IntervalAnalysisAbstractValue.Unknown => "U"
      case IntervalAnalysisAbstractValue.Bottom => "⊥"
    }
  }

class IntervalAnalysis(cfg: Cfg, maxIteration: Int = 5) extends ForwardMayAnalyzer[AbstractState[IntervalAnalysisAbstractValue]](cfg):
  override def entryAbstractState(): AbstractState[IntervalAnalysisAbstractValue] = AbstractState(cfg.parameters.map { parameter => parameter -> IntervalAnalysisAbstractValue.Unknown }.toMap)

  final override def analyseStatement(abstractState: AbstractState[IntervalAnalysisAbstractValue], statement: Statement): AbstractState[IntervalAnalysisAbstractValue] = {
    ???
  }

  final override def conditionUpdate(abstractState: AbstractState[IntervalAnalysisAbstractValue], condition: BooleanExpression): Option[AbstractState[IntervalAnalysisAbstractValue]] = {
    ???
  }

  override def merge(analysisState: AnalysisState[AbstractState[IntervalAnalysisAbstractValue]], node: ProgramPoint, left: AbstractState[IntervalAnalysisAbstractValue], right: AbstractState[IntervalAnalysisAbstractValue]): Try[AbstractState[IntervalAnalysisAbstractValue]] = {
    super.merge(analysisState, node, left, right)
  }
