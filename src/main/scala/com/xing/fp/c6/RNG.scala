package com.xing.fp.c6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    (if (i < 0) -(i + 1) else i, nextRng)
  }
}
