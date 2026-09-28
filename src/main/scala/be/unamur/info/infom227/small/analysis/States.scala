package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.cfg.ProgramPoint

import scala.collection.mutable

case class AbstractState[T <: Lattice[T]](variables: Map[String, T] = Map()) extends Lattice[AbstractState[T]]:
  def apply(variable: String): T =
    variables.get(variable) match {
      case Some(value) => value
      case None => throw new RuntimeException(s"Variable $variable not found")
    }

  def apply(update: (String, T)): AbstractState[T] = {
    AbstractState(variables + update)
  }

  override def toString: String = {
    val builder = new StringBuilder()
    for ((variable, abstractValue) <- variables) {
      builder.append(s"$variable: $abstractValue\n")
    }
    builder.toString()
  }

  override def join(other: AbstractState[T]): AbstractState[T] = {
    AbstractState(this.variables.foldLeft(other.variables) {
      case (acc, (name, newAbstractValue)) =>
        val mergedValue = acc.get(name) match {
          case Some(currentAbstractValue) => newAbstractValue.join(currentAbstractValue)
          case None => newAbstractValue
        }
        acc + (name -> mergedValue)
    })
  }

  override def meet(other: AbstractState[T]): AbstractState[T] = {
    AbstractState(this.variables.keys.toSet.intersect(other.variables.keys.toSet).map { name =>
      name -> this.variables(name).meet(other.variables(name))
    }.toMap)
  }

case class AnalysisState[S](var abstractStates: mutable.Map[ProgramPoint, S] = mutable.Map())
