package be.unamur.info.infom227.small.analysis

trait Lattice[L] {
  def join(other: L): L

  def meet(other: L): L
}
