package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.cfg.ProgramPoint

import scala.collection.mutable

private def joinMap[K, V](left: Map[K, V], right: Map[K, V], f: (V, V) => V): Map[K, V] = {
  left.foldLeft(right) {
    case (acc, (name, newAbstractValue)) =>
      val mergedValue = acc.get(name) match {
        case Some(currentAbstractValue) => f(newAbstractValue, currentAbstractValue)
        case None => newAbstractValue
      }
      acc + (name -> mergedValue)
  }
}

private def meetMap[K, V](left: Map[K, V], right: Map[K, V], f: (V, V) => V): Map[K, V] = {
  left.keys.toSet.intersect(right.keys.toSet).map { name =>
    name -> f(left(name), right(name))
  }.toMap
}

trait State[T]:
  def variables: Map[String, T]

class AbstractState[T <: Lattice[T]](val variables: Map[String, T] = Map()) extends Lattice[AbstractState[T]], State[T]:
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

  override def equals(obj: Any): Boolean = obj match {
    case other: AbstractState[_] => variables == other.variables
    case _ => false
  }

  override def hashCode(): Int = variables.hashCode()

  def joinWith(other: AbstractState[T], f: (T, T) => T): AbstractState[T] = {
    AbstractState(joinMap(this.variables, other.variables, f))
  }

  override def join(other: AbstractState[T]): AbstractState[T] = {
    joinWith(other, (left, right) => left.join(right))
  }

  def meetWith(other: AbstractState[T], f: (T, T) => T): AbstractState[T] = {
    AbstractState(meetMap(this.variables, other.variables, f))
  }

  override def meet(other: AbstractState[T]): AbstractState[T] = {
    meetWith(other, (left, right) => left.meet(right))
  }

class InterproceduralAbstractState[T <: Lattice[T]](val state: AbstractState[T], val calls: Map[String, Set[List[T]]], val returnValue: Option[T]) extends Lattice[InterproceduralAbstractState[T]], State[T]:
  def variables: Map[String, T] = state.variables

  def apply(variable: String): T =
    state(variable)

  def apply(update: (String, T)): InterproceduralAbstractState[T] = {
    InterproceduralAbstractState(state(update), calls, returnValue)
  }

  def withReturn(value: T): InterproceduralAbstractState[T] = {
    InterproceduralAbstractState(state, calls, Some(value))
  }

  def addCall(callee: String, arguments: List[T]): InterproceduralAbstractState[T] = {
    InterproceduralAbstractState(state, calls + (callee -> (calls.getOrElse(callee, Set()) + arguments)), returnValue)
  }

  override def toString: String = {
    val builder = new StringBuilder()
    for ((variable, abstractValue) <- variables) {
      builder.append(s"$variable: $abstractValue\n")
    }
    builder.toString()
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: InterproceduralAbstractState[_] => state == other.state && calls == other.calls && returnValue == other.returnValue
    case _ => false
  }

  override def hashCode(): Int = state.hashCode() + calls.hashCode() + returnValue.hashCode()

  override def join(other: InterproceduralAbstractState[T]): InterproceduralAbstractState[T] = {
    InterproceduralAbstractState(
      state.join(other.state),
      joinMap(calls, other.calls, (left, right) => left union right),
      (returnValue, other.returnValue) match {
        case (Some(left), Some(right)) => Some(left.join(right))
        case (Some(left), None) => Some(left)
        case (None, Some(right)) => Some(right)
        case (None, None) => None
      }
    )
  }

  override def meet(other: InterproceduralAbstractState[T]): InterproceduralAbstractState[T] = {
    InterproceduralAbstractState(
      state.meet(other.state),
      joinMap(calls, other.calls, (left, right) => left intersect right),
      (returnValue, other.returnValue) match {
        case (Some(left), Some(right)) => Some(left.meet(right))
        case (Some(left), None) => Some(left)
        case (None, Some(right)) => Some(right)
        case (None, None) => None
      }
    )
  }

class IterationAbstractState[T <: Lattice[T]](val state: AbstractState[T], val iterations: Map[ProgramPoint, Int] = Map()) extends Lattice[IterationAbstractState[T]], State[T]:
  def variables: Map[String, T] = state.variables

  def apply(variable: String): T =
    state(variable)

  def apply(update: (String, T)): IterationAbstractState[T] = {
    IterationAbstractState(state(update), iterations)
  }

  def increased(programPoint: ProgramPoint): IterationAbstractState[T] = {
    IterationAbstractState(state, iterations + (programPoint -> (iterations.getOrElse(programPoint, 0) + 1)))
  }

  override def toString: String = {
    val builder = new StringBuilder()
    for ((variable, abstractValue) <- variables) {
      builder.append(s"$variable: $abstractValue\n")
    }
    builder.toString()
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: IterationAbstractState[_] => state == other.state && iterations == other.iterations
    case _ => false
  }

  override def hashCode(): Int = state.hashCode() + iterations.hashCode()

  def joinWith(other: IterationAbstractState[T], f: (T, T, Map[ProgramPoint, Int]) => T): IterationAbstractState[T] = {
    val joinedIterations = joinMap(iterations, other.iterations, (left, right) => left max right)
    IterationAbstractState(state.joinWith(other.state, (left, right) => f(left, right, joinedIterations)), joinedIterations)
  }

  override def join(other: IterationAbstractState[T]): IterationAbstractState[T] = {
    joinWith(other, (left, right, _) => left.join(right))
  }

  def meetWith(other: IterationAbstractState[T], f: (T, T, Map[ProgramPoint, Int]) => T): IterationAbstractState[T] = {
    val joinedIterations = joinMap(iterations, other.iterations, (left, right) => left max right)
    IterationAbstractState(state.meetWith(other.state, (left, right) => f(left, right, joinedIterations)), joinedIterations)
  }

  override def meet(other: IterationAbstractState[T]): IterationAbstractState[T] = {
    meetWith(other, (left, right, _) => left.meet(right))
  }

case class AnalysisState[S](var abstractStates: mutable.Map[ProgramPoint, S] = mutable.Map())
