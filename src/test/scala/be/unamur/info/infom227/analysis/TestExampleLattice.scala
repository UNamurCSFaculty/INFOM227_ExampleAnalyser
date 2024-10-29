package be.unamur.info.infom227.analysis

import org.scalatest.funsuite.AnyFunSuite

enum SignAnalysisLattice:
  case Lt, Gt, Z, Lte, Gte, Nz, U, Bottom

class TestExampleLattice extends AnyFunSuite {

  private val lattice = ExampleFiniteSizeLattice(
    Set(
      (SignAnalysisLattice.Bottom, SignAnalysisLattice.Lt),
      (SignAnalysisLattice.Bottom, SignAnalysisLattice.Z),
      (SignAnalysisLattice.Bottom, SignAnalysisLattice.Gt),
      (SignAnalysisLattice.Lt, SignAnalysisLattice.Lte),
      (SignAnalysisLattice.Lt, SignAnalysisLattice.Nz),
      (SignAnalysisLattice.Z, SignAnalysisLattice.Lte),
      (SignAnalysisLattice.Z, SignAnalysisLattice.Gte),
      (SignAnalysisLattice.Gt, SignAnalysisLattice.Nz),
      (SignAnalysisLattice.Gt, SignAnalysisLattice.Gte),
      (SignAnalysisLattice.Lte, SignAnalysisLattice.U),
      (SignAnalysisLattice.Nz, SignAnalysisLattice.U),
      (SignAnalysisLattice.Gte, SignAnalysisLattice.U),
    )
  )

  test("valid bottom value") {
    val expected = Some(SignAnalysisLattice.Bottom)

    val result = lattice.bottom

    assert(expected == result)
  }

  test("valid top value") {
    val expected = Some(SignAnalysisLattice.U)

    val result = lattice.top

    assert(expected == result)
  }

  test("valid join values") {
    assert(lattice.join(SignAnalysisLattice.Bottom, SignAnalysisLattice.Bottom) == Some(SignAnalysisLattice.Bottom))
    assert(lattice.join(SignAnalysisLattice.Bottom, SignAnalysisLattice.Lt) == Some(SignAnalysisLattice.Lt))
    assert(lattice.join(SignAnalysisLattice.Bottom, SignAnalysisLattice.Z) == Some(SignAnalysisLattice.Z))
    assert(lattice.join(SignAnalysisLattice.Bottom, SignAnalysisLattice.Gt) == Some(SignAnalysisLattice.Gt))
    assert(lattice.join(SignAnalysisLattice.Bottom, SignAnalysisLattice.Nz) == Some(SignAnalysisLattice.Nz))
    assert(lattice.join(SignAnalysisLattice.Z, SignAnalysisLattice.Z) == Some(SignAnalysisLattice.Z))
    assert(lattice.join(SignAnalysisLattice.Z, SignAnalysisLattice.U) == Some(SignAnalysisLattice.U))
    assert(lattice.join(SignAnalysisLattice.U, SignAnalysisLattice.Z) == Some(SignAnalysisLattice.U))
    assert(lattice.join(SignAnalysisLattice.Gt, SignAnalysisLattice.Lte) == Some(SignAnalysisLattice.U))
    assert(lattice.join(SignAnalysisLattice.Lte, SignAnalysisLattice.Gt) == Some(SignAnalysisLattice.U))
    assert(lattice.join(SignAnalysisLattice.Gt, SignAnalysisLattice.Lt) == Some(SignAnalysisLattice.Nz))
    assert(lattice.join(SignAnalysisLattice.Lt, SignAnalysisLattice.Gt) == Some(SignAnalysisLattice.Nz))
  }
}
