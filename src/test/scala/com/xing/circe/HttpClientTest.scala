package com.xing.circe

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HttpClientTest extends AnyWordSpec with Matchers {

  "Http client" should {
    "get string pets" in {
      println(HttpClient.getStringPets().unsafeRunSync())
    }

    "get pets" in {
      val pets = HttpClient.getPets().unsafeRunSync()
      println(pets)
      pets.size shouldEqual 3
    }
  }
}
