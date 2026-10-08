package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast.*
import be.unamur.info.infom227.small.cfg.Cfg

enum SignAnalysisAbstractValue extends Lattice[SignAnalysisAbstractValue]:
  case Unknown
  case Bottom

  override def join(other: SignAnalysisAbstractValue): SignAnalysisAbstractValue = {
    (this, other) match {
      case (SignAnalysisAbstractValue.Bottom, _) => other
      case (_, SignAnalysisAbstractValue.Bottom) => this
      case _ => SignAnalysisAbstractValue.Unknown
    }
  }

  override def meet(other: SignAnalysisAbstractValue): SignAnalysisAbstractValue = {
    (this, other) match {
      case (SignAnalysisAbstractValue.Unknown, _) => other
      case (_, SignAnalysisAbstractValue.Unknown) => this
      case _ => SignAnalysisAbstractValue.Bottom
    }
  }

  override def toString: String = {
    this match {
      case SignAnalysisAbstractValue.Unknown => "U"
      case SignAnalysisAbstractValue.Bottom => "⊥"
    }
  }

class SignAnalysis(cfg: Cfg) extends ForwardMayAnalyzer[AbstractState[SignAnalysisAbstractValue]](cfg):
  override def entryAbstractState(): AbstractState[SignAnalysisAbstractValue] = AbstractState(cfg.parameters.map { parameter => parameter -> SignAnalysisAbstractValue.Unknown }.toMap)

  final override def analyseStatement(abstractState: AbstractState[SignAnalysisAbstractValue], statement: Statement): AbstractState[SignAnalysisAbstractValue] = {
    ???
  }

  final override def conditionUpdate(abstractState: AbstractState[SignAnalysisAbstractValue], condition: BooleanExpression): Option[AbstractState[SignAnalysisAbstractValue]] = {
    ???
  }
