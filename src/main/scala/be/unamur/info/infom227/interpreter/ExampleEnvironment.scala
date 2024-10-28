package be.unamur.info.infom227.interpreter

import java.io.{BufferedWriter, OutputStreamWriter}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class ExampleEnvironment[T] private (private val stdout: BufferedWriter, private val parentEnvironmentOption: Option[ExampleEnvironment[T]] = None) {

  def this(parentEnvironment: ExampleEnvironment[T]) = {
    this(parentEnvironment.stdout, Some(parentEnvironment))
  }

  def this(stdout: BufferedWriter) = {
    this(stdout, None)
  }

  def this() = {
    this(new BufferedWriter(new OutputStreamWriter(System.out)))
  }

  private val variables = mutable.HashMap[String, T]()

  def print(value: String): Try[Unit] = {
    Try {
      stdout.write(value)
      stdout.newLine()
      stdout.flush()
    }
  }

  def get(name: String, recursive: Boolean = true): Try[T] = {
    variables.get(name) match {
      case Some(value) => Success(value)
      case None => parentEnvironmentOption match {
        case Some(parentEnvironment) if recursive => parentEnvironment.get(name, recursive)
        case _ => Failure(new NoSuchElementException(s"Undefined variable : $name"))
      }
    }
  }

  def define(name: String, value: T): Try[Unit] = {
    if (variables.contains(name)) {
      Failure(new NoSuchElementException(s"Variable already defined : $name"))
    } else {
      variables.put(name, value)
      Success(())
    }
  }

  def set(name: String, value: T): Try[Unit] = {
    if (variables.contains(name)) {
      variables.put(name, value)
      Success(())
    } else {
      Failure(new NoSuchElementException(s"Undefined variable : $name"))
    }
  }

  override def toString: String = variables.toString()
}
