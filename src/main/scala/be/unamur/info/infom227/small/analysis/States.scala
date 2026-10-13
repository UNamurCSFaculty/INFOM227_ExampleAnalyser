package be.unamur.info.infom227.small.analysis

import be.unamur.info.infom227.small.cfg.ProgramPoint

import scala.collection.mutable

def joinMap[K, V](left: Map[K, V], right: Map[K, V], f: (V, V) => V): Map[K, V] = {
  left.foldLeft(right) {
    case (acc, (name, newAbstractValue)) =>
      val mergedValue = acc.get(name) match {
        case Some(currentAbstractValue) => f(newAbstractValue, currentAbstractValue)
        case None => newAbstractValue
      }
      acc + (name -> mergedValue)
  }
}

def meetMap[K, V](left: Map[K, V], right: Map[K, V], f: (V, V) => V): Map[K, V] = {
  left.keys.toSet.intersect(right.keys.toSet).map { name =>
    name -> f(left(name), right(name))
  }.toMap
}

def joinOption[T <: Lattice[T]](left: Option[T], right: Option[T]): Option[T] = {
  (left, right) match {
    case (Some(l), Some(r)) => Some(l.join(r))
    case (Some(l), None) => Some(l)
    case (None, Some(r)) => Some(r)
    case (None, None) => None
  }
}

def meetOption[T <: Lattice[T]](left: Option[T], right: Option[T]): Option[T] = {
  (left, right) match {
    case (Some(l), Some(r)) => Some(l.meet(r))
    case (Some(l), None) => Some(l)
    case (None, Some(r)) => Some(r)
    case (None, None) => None
  }
}

def joinList[T <: Lattice[T]](left: List[T], right: List[T]): List[T] = {
  left.zip(right).map((l, r) => l.join(r))
}

def meetList[T <: Lattice[T]](left: List[T], right: List[T]): List[T] = {
  left.zip(right).map((l, r) => l.meet(r))
}

trait State[T]:
  def variables: Map[String, T]

class AbstractState[T <: Lattice[T]](val variables: Map[String, T] = Map()) extends Lattice[AbstractState[T]], State[T]:
  def apply(variable: String): T = {
    variables.get(variable) match {
      case Some(value) => value
      case None => throw new RuntimeException(s"Variable $variable not found")
    }
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

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: AbstractState[_] => variables == other.variables
      case _ => false
    }
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

class ExtendedAbstractState[T <: Lattice[T]](
  val state: AbstractState[T],
  val calls: Map[String, List[T]] = Map.empty[String, List[T]],
  val returnValue: Option[T] = None
) extends Lattice[ExtendedAbstractState[T]], State[T]:
  def variables: Map[String, T] = state.variables

  def apply(variable: String): T = {
    state(variable)
  }

  def apply(update: (String, T)): ExtendedAbstractState[T] = {
    ExtendedAbstractState(state(update), calls, returnValue)
  }

  def withReturnValue(value: T): ExtendedAbstractState[T] = {
    ExtendedAbstractState(state, calls, Some(value))
  }

  def addCall(callee: String, arguments: List[T]): ExtendedAbstractState[T] = {
    ExtendedAbstractState(
      state,
      calls + (callee -> (calls.get(callee) match {
        case Some(previousArguments) => joinList(previousArguments, arguments)
        case None => arguments
      })),
      returnValue
    )
  }

  override def toString: String = {
    val builder = new StringBuilder()
    for ((variable, abstractValue) <- variables) {
      builder.append(s"$variable: $abstractValue\n")
    }
    builder.toString()
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: ExtendedAbstractState[_] => state == other.state && calls == other.calls && returnValue == other.returnValue
      case _ => false
    }
  }

  override def hashCode(): Int = state.hashCode() + calls.hashCode() + returnValue.hashCode()

  override def join(other: ExtendedAbstractState[T]): ExtendedAbstractState[T] = {
    ExtendedAbstractState(
      state.join(other.state),
      joinMap(calls, other.calls, (left, right) => joinList(left, right)),
      joinOption(returnValue, other.returnValue)
    )
  }

  override def meet(other: ExtendedAbstractState[T]): ExtendedAbstractState[T] = {
    ExtendedAbstractState(
      state.meet(other.state),
      meetMap(calls, other.calls, (left, right) => meetList(left, right)),
      meetOption(returnValue, other.returnValue)
    )
  }

class IterationAbstractState[T <: Lattice[T]](
  val state: AbstractState[T],
  val iterations: Map[ProgramPoint, Int] = Map()
) extends Lattice[IterationAbstractState[T]], State[T]:
  def variables: Map[String, T] = state.variables

  def apply(variable: String): T = {
    state(variable)
  }

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

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: IterationAbstractState[_] => state == other.state && iterations == other.iterations
      case _ => false
    }
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

class AnalysisState[S](val abstractStates: mutable.Map[ProgramPoint, S] = mutable.Map.empty[ProgramPoint, S])

case class InterproceduralAbstractState[S, O](
  name: String,
  abstractState: S,
  observer: O
) extends ObservableAbstractState[O]

case class FunctionState[T <: Lattice[T]](
  abstractState: AbstractState[T] = AbstractState[T](),
  arguments: List[T] = List.empty[T],
  returnValue: Option[T] = None,
  callees: Set[String] = Set.empty
) extends Lattice[FunctionState[T]]:
  override def join(other: FunctionState[T]): FunctionState[T] = {
    FunctionState(
      abstractState.join(other.abstractState),
      joinList(arguments, other.arguments),
      joinOption(returnValue, other.returnValue),
      callees.union(other.callees)
    )
  }

  override def meet(other: FunctionState[T]): FunctionState[T] = {
    FunctionState(
      abstractState.meet(other.abstractState),
      meetList(arguments, other.arguments),
      meetOption(returnValue, other.returnValue),
      callees.intersect(other.callees)
    )
  }

class InterproceduralAnalysisState[T <: Lattice[T]](val functions: Map[String, FunctionState[T]] = Map.empty[String, FunctionState[T]])
