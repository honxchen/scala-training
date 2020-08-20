package com.xing.fp.c6

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class SimpleRNGTest extends AnyWordSpec with Matchers{

  "RNG test" should {
    "nextInt" in {
      val rng = SimpleRNG(1)
      val (n1, rng2) = rng.nextInt
      n1 shouldEqual 384748
      rng2 shouldEqual SimpleRNG(25214903928L)
    }
  }
}