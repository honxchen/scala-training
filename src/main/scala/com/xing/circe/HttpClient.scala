package com.xing.circe

import cats.effect._
import org.http4s.client.blaze._
import scala.concurrent.ExecutionContext.global

case class Pet(id: Int, `type`: String, price: Float)
object HttpClient {
  implicit val cs: ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO] = IO.timer(global)
  val httpClient = BlazeClientBuilder[IO](global).resource

  def getStringPets() = {

  }

  def getPets() = {

  }
}
