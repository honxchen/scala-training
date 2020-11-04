package com.xing.fp.c4

import com.xing.fp.c3
import com.xing.fp.c3.List.tail
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

class OptionTest extends AnyWordSpec with Matchers{
    "Option" should {
      "lift function to option" in {
        Math.abs(-10) shouldEqual 10


        def liftAbs = OptionPractice.lift(Math.abs)

        liftAbs(Some(-10)) shouldEqual Some(10)
        liftAbs(None) shouldEqual None
      }

      "map2" in {
        def sum(a: Int, b: Int): Int = a + b
        def liftSum(a: Option[Int], b: Option[Int]): Option[Int] = OptionPractice.map2(a, b)(sum)

        sum(1, 2) shouldEqual 3
        liftSum(Some(1), Some(2)) shouldEqual Some(3)
        liftSum(Some(1), None) shouldEqual None
        liftSum(None, Some(2)) shouldEqual None
      }

      "sequence" in {
        val list = List(Some(1), Some(2), Some(3))

        OptionPractice.sequence(list) shouldEqual(Some(List(1, 2, 3)))
        OptionPractice.sequence(List(Some(1), Some(2), None)) shouldEqual None
      }

      "traverse" in {
        val list = List("1", "2", "3")

        def Try[A](a: => A): Option[A] = try Some(a) catch {case _ => None}
        OptionPractice.traverse[String, Int](list)(i => Try(i.toInt)) shouldEqual Some(List(1, 2, 3))

        val listwithWrong = List("a", "2", "3")

        OptionPractice.traverse[String, Int](listwithWrong)(i => Try(i.toInt)) shouldEqual None
      }
  }
}
