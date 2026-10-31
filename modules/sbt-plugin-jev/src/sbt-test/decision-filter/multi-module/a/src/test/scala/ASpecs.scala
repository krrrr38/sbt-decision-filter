package a

import org.scalatest.funsuite.AnyFunSuite

class AffectedSpec extends AnyFunSuite {
  test("uses Lib") {
    assert(Lib.value >= 1)
  }
}

class UnrelatedSpec extends AnyFunSuite {
  test("does not use Lib") {
    fail("this class should have been filtered out")
  }
}
