package com.xing.fp.c5

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StreamTest extends AnyWordSpec with Matchers {

  "Stream" should {
    val s = Stream(1, 2, 3, 4, 5)

    "headoption" in {
      s.headOption shouldEqual Some(1)
    }

    "toList" in {
      s.toList shouldEqual List(1, 2, 3, 4, 5)
    }

    "take" in {
      s.take(2).toList shouldEqual List(1, 2)
    }

    "drop" in {
      s.drop(2).toList shouldEqual List(3, 4, 5)
    }

    "takewhile" in {
      s.takeWhile(_ % 2 == 0).toList shouldEqual List(2, 4)
    }

    "take and then toList" in {
      Stream(1,2,3).take(2).toList shouldEqual List(1,2)
    }

    "exists" in {
      s.exists( _ == 2) shouldEqual true
    }

    "foldright" in {
      s.foldRight("6")(_+_) shouldEqual "123456"
    }

    "forall" in {
      s.forAll( _ == 2) shouldEqual false
      s.forAll( _ >= 1) shouldEqual true
    }

    "map" in {
      s.map(_+1).toList shouldEqual List(2,3,4,5,6)
    }

    "filter" in {
      s.filter(_!=2).toList shouldEqual List(1,3,4,5)
    }

    "append" in {
      s.append(Stream(6, 7)).toList shouldEqual List(1,2,3,4,5,6,7)
    }

    "flatmap" in {
      s.flatmap(v => Stream(v+1)).toList shouldEqual List(2,3,4,5,6)
    }
  }
}
