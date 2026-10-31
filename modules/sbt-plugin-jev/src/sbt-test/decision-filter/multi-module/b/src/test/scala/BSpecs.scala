package b

import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpec

class BAffectedSpec extends AnyWordSpec {
  "Lib" should {
    "be used" in {
      assert(a.Lib.value >= 1)
    }
  }
}

class BUnrelatedSpec extends AnyWordSpec with BeforeAndAfterAll {
  override def beforeAll(): Unit = throw new IllegalStateException("this class should have been filtered out")
  "Nothing" should {
    "run" in {
      fail("this class should have been filtered out")
    }
  }
}
