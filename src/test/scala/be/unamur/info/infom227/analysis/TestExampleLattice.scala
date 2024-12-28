package be.unamur.info.infom227.analysis

import org.scalatest.funsuite.AnyFunSuite

enum SignAnalysisAbstractValue:
  case Lt, Gt, Z, Lte, Gte, Nz, U, Bottom

object SignAnalysisAbstractValue {
  val lattice: ExampleLattice[SignAnalysisAbstractValue] = ExampleFiniteSizeLattice(
    Set(
      (SignAnalysisAbstractValue.Bottom, SignAnalysisAbstractValue.Lt),
      (SignAnalysisAbstractValue.Bottom, SignAnalysisAbstractValue.Z),
      (SignAnalysisAbstractValue.Bottom, SignAnalysisAbstractValue.Gt),
      (SignAnalysisAbstractValue.Lt, SignAnalysisAbstractValue.Lte),
      (SignAnalysisAbstractValue.Lt, SignAnalysisAbstractValue.Nz),
      (SignAnalysisAbstractValue.Z, SignAnalysisAbstractValue.Lte),
      (SignAnalysisAbstractValue.Z, SignAnalysisAbstractValue.Gte),
      (SignAnalysisAbstractValue.Gt, SignAnalysisAbstractValue.Nz),
      (SignAnalysisAbstractValue.Gt, SignAnalysisAbstractValue.Gte),
      (SignAnalysisAbstractValue.Lte, SignAnalysisAbstractValue.U),
      (SignAnalysisAbstractValue.Nz, SignAnalysisAbstractValue.U),
      (SignAnalysisAbstractValue.Gte, SignAnalysisAbstractValue.U),
    )
  )
}

class TestExampleLattice extends AnyFunSuite {

  test("valid bottom value") {
    val expected = Some(SignAnalysisAbstractValue.Bottom)

    val result = SignAnalysisAbstractValue.lattice.bottom

    assert(expected == result)
  }

  test("valid top value") {
    val expected = Some(SignAnalysisAbstractValue.U)

    val result = SignAnalysisAbstractValue.lattice.top

    assert(expected == result)
  }

  test("valid included values") {
    assert(!SignAnalysisAbstractValue.lattice.includes(SignAnalysisAbstractValue.Bottom, SignAnalysisAbstractValue.Z))
    assert(!SignAnalysisAbstractValue.lattice.includes(SignAnalysisAbstractValue.Bottom, SignAnalysisAbstractValue.U))
    assert(!SignAnalysisAbstractValue.lattice.includes(SignAnalysisAbstractValue.Nz, SignAnalysisAbstractValue.Z))
    assert(SignAnalysisAbstractValue.lattice.includes(SignAnalysisAbstractValue.U, SignAnalysisAbstractValue.Z))
    assert(SignAnalysisAbstractValue.lattice.includes(SignAnalysisAbstractValue.Z, SignAnalysisAbstractValue.Z))
    assert(SignAnalysisAbstractValue.lattice.includes(SignAnalysisAbstractValue.Gte, SignAnalysisAbstractValue.Z))
    assert(SignAnalysisAbstractValue.lattice.includes(SignAnalysisAbstractValue.Gte, SignAnalysisAbstractValue.Gt))
  }

  test("valid join values") {
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.Bottom, SignAnalysisAbstractValue.Bottom) == Some(SignAnalysisAbstractValue.Bottom))
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.Bottom, SignAnalysisAbstractValue.Lt) == Some(SignAnalysisAbstractValue.Lt))
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.Bottom, SignAnalysisAbstractValue.Z) == Some(SignAnalysisAbstractValue.Z))
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.Bottom, SignAnalysisAbstractValue.Gt) == Some(SignAnalysisAbstractValue.Gt))
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.Bottom, SignAnalysisAbstractValue.Nz) == Some(SignAnalysisAbstractValue.Nz))
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.Z, SignAnalysisAbstractValue.Z) == Some(SignAnalysisAbstractValue.Z))
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.Z, SignAnalysisAbstractValue.U) == Some(SignAnalysisAbstractValue.U))
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.U, SignAnalysisAbstractValue.Z) == Some(SignAnalysisAbstractValue.U))
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.Gt, SignAnalysisAbstractValue.Lte) == Some(SignAnalysisAbstractValue.U))
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.Lte, SignAnalysisAbstractValue.Gt) == Some(SignAnalysisAbstractValue.U))
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.Gt, SignAnalysisAbstractValue.Lt) == Some(SignAnalysisAbstractValue.Nz))
    assert(SignAnalysisAbstractValue.lattice.join(SignAnalysisAbstractValue.Lt, SignAnalysisAbstractValue.Gt) == Some(SignAnalysisAbstractValue.Nz))
  }

  test("valid meet values") {
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.U, SignAnalysisAbstractValue.U) == Some(SignAnalysisAbstractValue.U))
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.U, SignAnalysisAbstractValue.Lt) == Some(SignAnalysisAbstractValue.Lt))
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.U, SignAnalysisAbstractValue.Z) == Some(SignAnalysisAbstractValue.Z))
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.U, SignAnalysisAbstractValue.Gt) == Some(SignAnalysisAbstractValue.Gt))
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.U, SignAnalysisAbstractValue.Nz) == Some(SignAnalysisAbstractValue.Nz))
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.Z, SignAnalysisAbstractValue.Z) == Some(SignAnalysisAbstractValue.Z))
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.Z, SignAnalysisAbstractValue.Bottom) == Some(SignAnalysisAbstractValue.Bottom))
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.Bottom, SignAnalysisAbstractValue.Z) == Some(SignAnalysisAbstractValue.Bottom))
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.Gte, SignAnalysisAbstractValue.Lte) == Some(SignAnalysisAbstractValue.Z))
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.Lte, SignAnalysisAbstractValue.Gte) == Some(SignAnalysisAbstractValue.Z))
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.Gt, SignAnalysisAbstractValue.Lte) == Some(SignAnalysisAbstractValue.Bottom))
    assert(SignAnalysisAbstractValue.lattice.meet(SignAnalysisAbstractValue.Lte, SignAnalysisAbstractValue.Gt) == Some(SignAnalysisAbstractValue.Bottom))
  }
}
