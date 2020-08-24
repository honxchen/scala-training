package com.xing.circe

import org.http4s.Headers

case class Event(name: String)
case class HttpEvent(url: String, headers: Headers)

object JsonHelper {

  def string2Json(string: String) = ???

  def liststring2Json(list: List[String]) = ???

  def event2Json(event: Event) = ???

  def headerEvent2Json(headerEvent: HttpEvent) = ???
}
