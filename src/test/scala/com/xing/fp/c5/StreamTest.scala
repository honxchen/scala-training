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
  }
}
