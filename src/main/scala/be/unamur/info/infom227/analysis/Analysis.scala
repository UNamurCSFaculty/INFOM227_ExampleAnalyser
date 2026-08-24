package be.unamur.info.infom227.analysis

import scala.util.Try
import scala.collection.mutable
import scala.util.control.Breaks._

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

trait AnalysisObserver[N, A] {
  def beforeAnalysis(analysisState: A, worklist: mutable.Set[N]): Unit = {}
  def beforeIteration(analysisState: A, worklist: mutable.Set[N]): Unit = {}
  def beforeNodeAnalysis(analysisState: A, worklist: mutable.Set[N], node: N): Unit = {}
  def afterNodeAnalysis(analysisState: A, worklist: mutable.Set[N], node: N): Unit = {}
  def afterIteration(analysisState: A, worklist: mutable.Set[N]): Unit = {}
  def afterAnalysis(analysisState: A, worklist: mutable.Set[N]): Unit = {}
}

class DummyObserver[N, A] extends AnalysisObserver[N, A]

def analysis[N, S, A, G <: GraphAnalyser[N, S, A], O <: AnalysisObserver[N, A]](analyser: G, observer: O): Try[A] = {
  Try {
    val analysisState = analyser.initialiseAnalysisState().get

    val worklist = mutable.Set.from(analyser.entryNodes)

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

        observer.afterNodeAnalysis(analysisState, worklist, node)

        analyser.optimise(analysisState, worklist)

        observer.afterIteration(analysisState, worklist)
      }
    }

    observer.afterAnalysis(analysisState, worklist)

    analysisState
  }
}
