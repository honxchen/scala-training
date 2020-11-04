package com.xing.fp.c4

object OptionPractice {

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    xs.length match {
      case 0 => None
      case _ => Some(xs.sum / xs.length)
    }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](list: List[Option[A]]): Option[List[A]] = list match {
    case Nil => None
    case _ =>  list.foldLeft(Option(List.empty): Option[List[A]])(map2(_, _)(_ :+ _))
  }

  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = list match {
    case Nil => None
    case _ => list.foldLeft(Option(List.empty): Option[List[B]])((somelist: Option[List[B]], a: A) => map2(somelist, f(a))(_ :+ _))
  }

}
