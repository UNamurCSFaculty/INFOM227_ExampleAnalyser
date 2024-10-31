package be.unamur.info.infom227.analysis

import be.unamur.info.infom227.cfg.{ExampleCfg, ExampleProgramPoint}

import scala.util.Try

enum ExampleErrorMessageType:
  case Error, Warning

abstract class ExampleResultsInterpreter[L](processingRules: (ExampleProgramPoint, Map[ExampleProgramPoint, ExampleAbstractEnvironment[String, L]]) => Option[(ExampleErrorMessageType, String)]) {
  def interpret(cfg: ExampleCfg, results: Map[ExampleProgramPoint, ExampleAbstractEnvironment[String, L]]): Try[List[(Int, ExampleErrorMessageType, String)]] = {
    Try {
      val entryPoint = cfg.entryPoint.get

      val programPointDone = scala.collection.mutable.Set[ExampleProgramPoint]()
      val programPointToInterpret = scala.collection.mutable.Set[ExampleProgramPoint](entryPoint)
      val messages = scala.collection.mutable.ArrayBuffer[(Int, ExampleErrorMessageType, String)]()

      while (programPointToInterpret.nonEmpty) {
        val programPoint = programPointToInterpret.head
        programPointToInterpret.remove(programPoint)

        processingRules(programPoint, results).map((messageType, message) =>
          messages.append((programPoint.statement.lineNumber, messageType, message))
        )

        programPointDone.add(programPoint)

        programPointToInterpret.addAll(cfg.succ(programPoint).removedAll(programPointDone))
      }

      messages.toList
    }
  }
}
