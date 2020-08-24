package com.xing.circe

import io.circe.Encoder.encodeMap
import io.circe.{Encoder, Json}
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._
import org.http4s.{Header, Headers}

case class Event(name: String)
case class HttpEvent(url: String, headers: Headers)

object JsonHelper {

//  implicit val encodeFoo: Encoder[Headers] = new Encoder[Headers] {
//    final def apply(a: Headers): Json = a.toList.map(h => (h.name.toString(), h.value)).toMap.asJson
//  }

//  import io.circe.KeyEncoder.encodeKeyString
//  import io.circe.Encoder.encodeString
//  implicit val encodeFoo: Encoder[Headers] = encodeMap(encodeKeyString, encodeString).contramap(a => a.toList.map(h => (h.name.toString(), h.value)).toMap)

//  implicit def encodeFoo(implicit mapEncoder: Encoder[Map[String, String]]): Encoder[Headers] = mapEncoder.contramap(a => a.toList.map(h => (h.name.toString(), h.value)).toMap)

  def string2Json(string: String) = parse(string).right.get

  def liststring2Json(list: List[String]) = list.asJson

  def event2Json(event: Event) = event.asJson

  def headerEvent2Json(headerEvent: HttpEvent) = headerEvent.asJson
}
