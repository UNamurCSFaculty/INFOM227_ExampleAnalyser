package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.ast.{BooleanExpression, Statement}
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


abstract class SimpleAnalyzer[T <: Lattice[T]](cfg: Cfg) extends GraphAnalyser[ProgramPoint, AbstractState[T], AnalysisState[T]]:
  def bottom(): T

  def top(): T

  def analyseStatement(abstractState: AbstractState[T], statement: Statement): AbstractState[T]

  def conditionUpdate(abstractState: AbstractState[T], condition: BooleanExpression): Option[AbstractState[T]]

  override def entryNodes: Set[ProgramPoint] = cfg.entryPoints

  override def nextNodes(abstractState: AbstractState[T], node: ProgramPoint): Try[Set[ProgramPoint]] = Success(cfg.successors(node))

  override def initialiseAnalysisState(): Try[AnalysisState[T]] = Success(AnalysisState(mutable.Map(ProgramPoint.EntryPoint -> AbstractState(cfg.parameters.map { parameter => parameter -> top() }.toMap))))

  override def analyseNode(analysisState: AnalysisState[T], node: ProgramPoint): Try[AbstractState[T]] = {
    val abstractState = analysisState.abstractStates.getOrElse(node, AbstractState())

    node match {
      case ProgramPoint.StatementPoint(statement) => Try(analyseStatement(abstractState, statement))
      case _ => Success(abstractState)
    }
  }

  override def updateAbstractState(analysisState: AnalysisState[T], from: ProgramPoint, to: ProgramPoint, abstractState: AbstractState[T]): Try[Option[AbstractState[T]]] =
    cfg.condition(from, to) match {
      case Some(condition) => Try(conditionUpdate(abstractState, condition))
      case _ => Failure(new RuntimeException("condition should always exist"))
    }

  override def getAbstractState(analysisState: AnalysisState[T], node: ProgramPoint): Try[Option[AbstractState[T]]] = Success(analysisState.abstractStates.get(node))

  override def setAbstractState(analysisState: AnalysisState[T], node: ProgramPoint, abstractState: AbstractState[T]): Try[Unit] = {
    analysisState.abstractStates.addOne(node -> abstractState)
    Success(())
  }

  override def merge(analysisState: AnalysisState[T], node: ProgramPoint, left: AbstractState[T], right: AbstractState[T]): Try[AbstractState[T]] = {
    Success(AbstractState(left.variables.foldLeft(right.variables) { (acc, entry) =>
      val (name, newAbstractValue) = entry
      val mergedValue = acc.get(name) match {
        case Some(currentAbstractValue) => newAbstractValue.join(currentAbstractValue)
        case None => newAbstractValue
      }
      acc + (name -> mergedValue)
    }))
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

        for { nextNode <- analyser.nextNodes(abstractState, node).get } {
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
