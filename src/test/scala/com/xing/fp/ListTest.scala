package com.xing.fp

import com.xing.fp.List.tail
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ListTest extends AnyWordSpec with Matchers{

    "List" should {
      "sum" in {
        List.sum(List(1, 2, 3, 4, 5)) shouldEqual 15
      }

      "product" in {
        List.product(List(1.0,2.0,3.0,4.0,5.0))  shouldEqual 120
      }

      "set head" in {
        List.setHead(List(1, 2, 3, 4, 5), 2) shouldEqual List(2,2,3,4,5)
      }

      "get tail " in {
        tail(List(1, 2, 3, 4, 5)) shouldEqual List(2,3,4,5)
      }

      "drop " in {
        List.drop(List(1, 2, 3, 4, 5), 4) shouldEqual List(5)
      }

      "drop while" in {
        List.dropWhile(List(1, 2, 3, 2, 5))(x => x == 2) shouldEqual List(1, 3, 5)
      }

      "init" in {
        List.init(List(1,2,3,4,5)) shouldEqual List(1,2,3,4)
      }
  }
}
