package com.xing.fp

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def foldRight[A, B](l: List[A], init: B)(f: (A, B) => B): B = l match {
    case Nil => init
    case Cons(x, xs) => f(x, foldRight(xs, init)(f))
  }

  def foldLeft[A, B](l: List[A], init: B)(f: (B, A) => B) : B = l match {
    case Nil => init
    case Cons(x, xs) => foldLeft(xs, f(init, x))(f)
  }

  def reverse[A](l : List[A]): List[A] = foldRight(l, Nil: List[A])((t: A, h: List[A]) => List.append(h, List(t)))

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, a2) => Cons(a, a2))

  def length[A](l : List[A]): Int = foldLeft(l, 0)((t, _) => 1 + t)

  def tail[A](ints: List[A]): List[A] = ints match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](list: List[A], head: A): List[A] = list match {
    case Nil => Nil
    case Cons(_, t) => Cons(head, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case 1 => tail(l)
    case _ => drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case Cons(h, t) if !f(h) => Cons(h, dropWhile(t)(f))
  }

  def init[A](l: List[A]) : List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}
