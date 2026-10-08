package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast.*
import be.unamur.info.infom227.small.cfg.Cfg

enum ConstantAnalysisAbstractValue extends Lattice[ConstantAnalysisAbstractValue]:
  case Unknown
  case Bottom

  override def join(other: ConstantAnalysisAbstractValue): ConstantAnalysisAbstractValue = {
    (this, other) match {
      case (ConstantAnalysisAbstractValue.Bottom, _) => other
      case (_, ConstantAnalysisAbstractValue.Bottom) => this
      case _ => ConstantAnalysisAbstractValue.Unknown
    }
  }

  override def meet(other: ConstantAnalysisAbstractValue): ConstantAnalysisAbstractValue = {
    (this, other) match {
      case (ConstantAnalysisAbstractValue.Unknown, _) => other
      case (_, ConstantAnalysisAbstractValue.Unknown) => this
      case _ => ConstantAnalysisAbstractValue.Bottom
    }
  }

  override def toString: String = {
    this match {
      case ConstantAnalysisAbstractValue.Unknown => "U"
      case ConstantAnalysisAbstractValue.Bottom => "⊥"
    }
  }

class ConstantAnalysis(cfg: Cfg) extends ForwardMayAnalyzer[AbstractState[ConstantAnalysisAbstractValue]](cfg):
  override def entryAbstractState(): AbstractState[ConstantAnalysisAbstractValue] = AbstractState(cfg.parameters.map { parameter => parameter -> ConstantAnalysisAbstractValue.Unknown }.toMap)

  final override def analyseStatement(abstractState: AbstractState[ConstantAnalysisAbstractValue], statement: Statement): AbstractState[ConstantAnalysisAbstractValue] = {
    ???
  }

  final override def conditionUpdate(abstractState: AbstractState[ConstantAnalysisAbstractValue], condition: BooleanExpression): Option[AbstractState[ConstantAnalysisAbstractValue]] = {
    ???
  }
