package com.xing.fp.c4

class OptionPractice {

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    xs.length match {
      case 0 => None
      case _ => Some(xs.sum / xs.length)
    }
  }

}
