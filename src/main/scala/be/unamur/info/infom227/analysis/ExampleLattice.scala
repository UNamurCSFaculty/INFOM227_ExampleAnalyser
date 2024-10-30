package be.unamur.info.infom227.analysis


trait ExampleLattice[L] {
  def top: Option[L]

  def bottom: Option[L]

  def includes(including: L, included: L): Boolean

  def join(value1: L, value2: L): Option[L]
}

case class ExampleFiniteSizeLattice[L](edges: Set[(L, L)]) extends ExampleLattice[L] {
  def top: Option[L] = {
    edges.map(_._2).find(end => !edges.map(_._1).contains(end))
  }

  def bottom: Option[L] = {
    edges.map(_._1).find(start => !edges.map(_._2).contains(start))
  }

  private def ends(value: L): Set[L] = {
    edges.filter(_._1 == value).map(_._2)
  }

  def includes(including: L, included: L): Boolean = {
    val includedEnds = ends(included)

    including == included || includedEnds.contains(including) || includedEnds.exists(includes(including, _))
  }

  def join(value1: L, value2: L): Option[L] = {
    joinDist(value1, value2).map(_._1)
  }

  private def joinDist(value1: L, value2: L): Option[(L, Int)] = {
    val ends1 = ends(value1)
    val ends2 = ends(value2)

    if (value1 == value2) {
      Some((value1, 0))
    } else {
      val option1 = ends1.foldLeft(None: Option[(L, Int)]) {
        case (None, end1) => joinDist(end1, value2)
        case (Some((acc, dist)), end1) => joinDist(end1, value2)
          .map((result1, dist1) => if (dist1 < dist) (result1, dist1) else (acc, dist))
      }
      val option2 = ends2.foldLeft(None: Option[(L, Int)]) {
        case (None, end2) => joinDist(value1, end2)
        case (Some((acc, dist)), end2) => joinDist(value1, end2)
          .map((result2, dist2) => if (dist2 < dist) (result2, dist2) else (acc, dist))
      }
      (option1, option2) match
        case (Some((result1, dist1)), Some((_, dist2))) if dist1 < dist2 => Some((result1, dist1 + 1))
        case (Some((_, dist1)), Some((result2, dist2))) if dist1 > dist2 => Some((result2, dist2 + 1))
        case (Some((result1, dist1)), Some(_)) => Some((result1, dist1 + 1))
        case (Some((result1, dist1)), None) => Some((result1, dist1 + 1))
        case (None, Some((result2, dist2))) => Some((result2, dist2 + 1))
        case (None, None) => None
    }
  }
}
