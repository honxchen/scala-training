package com.xing.fp.c3

import com.xing.fp.c3
import com.xing.fp.c3.List.tail
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListTest extends AnyWordSpec with Matchers{
    "List" should {
      "sum" in {
        c3.List.sum(c3.List(1, 2, 3, 4, 5)) shouldEqual 15
      }
      "map " in {
        c3.List.map(c3.List(1, 2, 3))(_ + 1) shouldEqual c3.List(2, 3 ,4)
      }

      "flatmap " in {
        c3.List.flatMap(c3.List(1, 2, 3))(i => c3.List(i, i)) shouldEqual c3.List(1, 1, 2, 2, 3 ,3)
      }

      "product" in {
        c3.List.product(c3.List(1.0,2.0,3.0,4.0,5.0))  shouldEqual 120
      }

      "length" in {
        c3.List.length(c3.List(1, 2, 3, 4 ,5)) shouldEqual 5
      }

      "reverse" in {
        c3.List.reverse(c3.List(1, 2, 3, 4)) shouldEqual c3.List(4, 3, 2, 1)
      }

      "append" in {
        c3.List.append(c3.List(1, 2, 3), c3.List(4, 5, 6)) shouldEqual c3.List(1, 2, 3, 4, 5, 6)
      }

      "set head" in {
        c3.List.setHead(c3.List(1, 2, 3, 4, 5), 2) shouldEqual c3.List(2,2,3,4,5)
      }

      "get tail " in {
        tail(c3.List(1, 2, 3, 4, 5)) shouldEqual c3.List(2,3,4,5)
      }

      "drop " in {
        c3.List.drop(c3.List(1, 2, 3, 4, 5), 4) shouldEqual c3.List(5)
      }

      "drop while" in {
        c3.List.dropWhile(c3.List(1, 2, 3, 2, 5))(x => x == 2) shouldEqual c3.List(1, 3, 5)
      }

      "init" in {
        c3.List.init(c3.List(1,2,3,4,5)) shouldEqual c3.List(1,2,3,4)
      }
  }
}
