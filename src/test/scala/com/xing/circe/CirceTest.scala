package com.xing.circe

import com.xing.circe.JsonHelper.string2Json
import org.http4s.{Header, Headers}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CirceTest extends AnyWordSpec with Matchers {

  "circe" should {
    "parse string to json" in {
      val string: String = """
                          {
                            "foo": "bar",
                            "baz": 123
                          }
                          """
      val Json = string2Json(string)
      println(Json)
    }

    "encoder list string to json" in {
      val list = List("a", "b", "c")
      val Json = JsonHelper.liststring2Json(list)
      println(Json)
      //find the implicit encoder
    }

    "encoder case class to json" in {
      val event = Event("testevent")
      val json = JsonHelper.event2Json(event)
      println(json)
      // use auto
      // use semi-auto
      //decode
    }

    "encode cass class with class to json" in {
      val headerEvent = HttpEvent("headerEvent", Headers.of(Header("k1", "v1"), Header("k2", "v2")))
      val json = JsonHelper.headerEvent2Json(headerEvent)
      println(json)
      //use custom codec
      // use mapEncode
    }

    "decode string to case cass with class" in {
      //use mapDecode
    }
  }
}
