package com.xing.circe

import cats.effect._
import io.circe.Json
import org.http4s.client.blaze._
import org.http4s.client._

import scala.concurrent.ExecutionContext.global

case class Pet(id: Int, `type`: String, price: Float)
object HttpClient {
  import scala.concurrent.ExecutionContext.global
  implicit val cs: ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO] = IO.timer(global)
  val httpClient = BlazeClientBuilder[IO](global).resource
  def getStringPets() = {
    httpClient.use { client =>
      client.expect[String]("http://petstore-demo-endpoint.execute-api.com/petstore/pets")
    }
  }

  def getPets() = {
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityDecoder._
    httpClient.use { client =>
      client.expect[List[Pet]]("http://petstore-demo-endpoint.execute-api.com/petstore/pets")
    }
  }
}
