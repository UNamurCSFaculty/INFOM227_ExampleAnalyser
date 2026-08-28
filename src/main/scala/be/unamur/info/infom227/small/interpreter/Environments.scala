package be.unamur.info.infom227.small.interpreter

import scala.collection.immutable.HashMap
import scala.util.{Failure, Success, Try}

class Environments[T](val environments: List[HashMap[String, T]] = List.empty) {
  def get(name: String): Try[T] = {
    val value = for {
      head <- environments.headOption
      value <- head.get(name)
    } yield value

    value match {
      case Some(v) => Success(v)
      case None => Failure(new NoSuchElementException(s"Undefined variable : $name"))
    }
  }

  def updated(name: String, value: T): Try[Environments[T]] = {
    environments match {
      case Nil => Failure(new IllegalStateException(s"Cannot set variable $name because there is no environment"))
      case head :: tail => Success(Environments(head.updated(name, value) :: tail))
    }
  }

  def push(environment: HashMap[String, T] = HashMap.empty): Environments[T] = {
    Environments(environment :: environments)
  }

  def pop(): Try[Environments[T]] = {
    environments match {
      case Nil => Failure(new IllegalStateException("Cannot pop environment because there is no environment"))
      case _ :: tail => Success(Environments(tail))
    }
  }

  override def toString: String = environments.toString()
}
