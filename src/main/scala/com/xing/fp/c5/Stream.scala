package com.xing.fp.c5

import scala.collection.immutable

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => (t().toList).::(h())
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def take(n: Int): Stream[A] = n match {
    case 0 => Stream.empty
    case 1 => this  match {
      case Empty => Stream.empty
      case Cons(h, t) => Stream(h())
    }
    case _ => this  match {
      case Empty => Stream.empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }
  }

  def drop(n: Int): Stream[A] = n match {
    case 0 => this
    case 1 => this match {
      case Empty => Stream.empty
      case Cons(h, t) => t()
    }
    case _ => this match {
      case Empty => Stream.empty
      case Cons(h, t) => t().drop(n - 1)
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Stream.empty
    case Cons(h, t) =>  {
      if (p(h())) Cons(h, () => t().takeWhile(p)) else t().takeWhile(p)
    }
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
