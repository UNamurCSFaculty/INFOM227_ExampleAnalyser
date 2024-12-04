package be.unamur.info.infom227.ast

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class ExampleSymbolTable(private val parentSymbolTableOption: Option[ExampleSymbolTable] = None) {

  private val symbols = mutable.Map[String, ExampleType]()

  def variables: Map[String, ExampleType] = {
    symbols.toMap
  }

  def get(name: String, recursive: Boolean = true): Try[ExampleType] = {
    symbols.get(name) match {
      case Some(value) => Success(value)
      case None => parentSymbolTableOption match {
        case Some(parentSymbolTable) if recursive => parentSymbolTable.get(name, recursive)
        case _ => Failure(new NoSuchElementException(s"Undefined variable : $name"))
      }
    }
  }

  def define(name: String, exampleType: ExampleType): Try[Unit] = {
    if (symbols.contains(name)) {
      Failure(new IllegalArgumentException(s"Variable already defined : $name"))
    } else {
      symbols.put(name, exampleType)
      Success(())
    }
  }

  override def toString: String = symbols.toString()
}
