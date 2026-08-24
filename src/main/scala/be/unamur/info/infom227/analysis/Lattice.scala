package be.unamur.info.infom227.analysis


trait Lattice[L] {
  def join(other: L): L

  def meet(other: L): L
}
