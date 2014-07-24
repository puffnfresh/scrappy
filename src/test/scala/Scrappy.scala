package scrappy

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object Scrappy extends Properties("Scrappy") {
  property("identity") = forAll { (o: Option[Int]) =>
    val actual = doo {
      a <-- o
      Some(a)
    }
    val expected = o
    actual == expected
  }

  property("two binds") = forAll { (o: Option[Int], p: Option[Int]) =>
    val actual = doo {
      a <-- o
      b <-- p
      Some(a + b)
    }
    val expected = o.flatMap(a => p.flatMap(b => Some(a + b)))
    actual == expected
  }

  property("nested") = forAll { (o: Option[Int], p: Option[Int]) =>
    val actual = doo {
      a <-- o
      c <-- doo {
        b <-- p
        Some(b + 1)
      }
      Some(a + c)
    }
    val expected = o.flatMap(a => p.flatMap(b => Some(a + b + 1)))
    actual == expected
  }

  property("patterns") = forAll { (o: Option[Unit], p: Option[Int]) =>
    val actual = doo {
      () <-- o
      p
    }
    val expected = o.flatMap { case () => p }
    actual == expected
  }
}
