package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast.{BooleanExpression, Statement}
import be.unamur.info.infom227.small.cfg.ProgramPoint.{EntryPoint, ExitPoint}
import be.unamur.info.infom227.small.cfg.{Cfg, ProgramPoint}

import scala.util.{Failure, Success, Try}
import scala.collection.mutable
import scala.util.control.Breaks.*

trait GraphAnalyser[N, S, A] {
  def entryNodes: Set[N]

  def nextNodes(abstractState: S, node: N): Try[Set[N]]

  def initialiseAnalysisState(): Try[A]

  def analyseNode(analysisState: A, node: N): Try[S]

  def updateAbstractState(analysisState: A, from: N, to: N, abstractState: S): Try[Option[S]]

  def getAbstractState(analysisState: A, node: N): Try[Option[S]]

  def setAbstractState(analysisState: A, node: N, abstractState: S): Try[Unit]

  def merge(analysisState: A, node: N, left: S, right: S): Try[S]

  def optimise(analysisState: A, worklist: mutable.Set[N]): Try[Unit] = Try(())
}


abstract class ForwardMayAnalyzer[S <: Lattice[S]](cfg: Cfg) extends GraphAnalyser[ProgramPoint, S, AnalysisState[S]]:
  def entryAbstractState(): S

  def analyseStatement(abstractState: S, statement: Statement): S

  def conditionUpdate(abstractState: S, condition: BooleanExpression): Option[S]

  override def entryNodes: Set[ProgramPoint] = cfg.entryPoints

  override def nextNodes(abstractState: S, node: ProgramPoint): Try[Set[ProgramPoint]] = Success(cfg.successors(node))

  override def initialiseAnalysisState(): Try[AnalysisState[S]] = Success(AnalysisState(mutable.Map()))

  override def analyseNode(analysisState: AnalysisState[S], node: ProgramPoint): Try[S] = {
    node match {
      case EntryPoint => Success(entryAbstractState())
      case ProgramPoint.StatementPoint(statement) => Try(analyseStatement(analysisState.abstractStates(node), statement))
      case ExitPoint => Try(analysisState.abstractStates(node))
    }
  }

  override def updateAbstractState(analysisState: AnalysisState[S], from: ProgramPoint, to: ProgramPoint, abstractState: S): Try[Option[S]] =
    cfg.condition(from, to) match {
      case Some(condition) => Try(conditionUpdate(abstractState, condition))
      case _ => Failure(new RuntimeException("condition should always exist"))
    }

  override def getAbstractState(analysisState: AnalysisState[S], node: ProgramPoint): Try[Option[S]] = Success(analysisState.abstractStates.get(node))

  override def setAbstractState(analysisState: AnalysisState[S], node: ProgramPoint, abstractState: S): Try[Unit] = {
    analysisState.abstractStates.addOne(node -> abstractState)
    Success(())
  }

  override def merge(analysisState: AnalysisState[S], node: ProgramPoint, left: S, right: S): Try[S] = {
    Success(left.join(right))
  }

def analysis[N: Ordering, S, A, G <: GraphAnalyser[N, S, A], O <: AnalysisObserver[N, S, A]](analyser: G, observer: O): Try[A] = {
  Try {
    val analysisState = analyser.initialiseAnalysisState().get

    val worklist = mutable.SortedSet.from(analyser.entryNodes)

    observer.beforeAnalysis(analysisState, worklist)

    breakable {
      while (true) {
        observer.beforeIteration(analysisState, worklist)

        val node = worklist.headOption match {
          case Some(n) =>
            worklist.remove(n)
            n
          case None => break()
        }

        observer.beforeNodeAnalysis(analysisState, worklist, node)

        val abstractState = analyser.analyseNode(analysisState, node).get

        for {nextNode <- analyser.nextNodes(abstractState, node).get} {
          analyser.updateAbstractState(analysisState, node, nextNode, abstractState).get match {
            case None =>
            case Some(updatedAbstractState) =>
              val (shouldUpdate, newAbstractState) = analyser.getAbstractState(analysisState, nextNode).get match {
                case Some(nextNodeAbstractState) =>
                  val newAbstractState = analyser.merge(
                    analysisState,
                    nextNode,
                    nextNodeAbstractState,
                    updatedAbstractState
                  ).get
                  (
                    newAbstractState != nextNodeAbstractState,
                    newAbstractState
                  )
                case None =>
                  (true, updatedAbstractState)
              }

              if (shouldUpdate) {
                analyser.setAbstractState(analysisState, nextNode, newAbstractState)
                worklist.add(nextNode)
              }
          }
        }

        observer.afterNodeAnalysis(analysisState, abstractState, worklist, node)

        analyser.optimise(analysisState, worklist)

        observer.afterIteration(analysisState, worklist)
      }
    }

    observer.afterAnalysis(analysisState, worklist)

    analysisState
  }
}

def cfgsAnalysis[N: Ordering, S, A, G <: GraphAnalyser[N, S, A], O <: AnalysisObserver[N, S, A]](cfgs: Map[String, Cfg], analyserFactory: (name: String, cfg: Cfg) => Try[G], observerFactory: (name: String, cfg: Cfg) => Try[O]): Try[Map[String, (A, O)]] = {
  cfgs.foldLeft(Try(Map.empty[String, (A, O)])) { (acc, entry) =>
    for {
      results <- acc
      (name, cfg) = entry
      analyser <- analyserFactory(name, cfg)
      observer <- observerFactory(name, cfg)
      analysisState <- analysis(analyser, observer)
    } yield results + (name -> (analysisState, observer))
  }
}
