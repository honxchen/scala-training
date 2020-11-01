package com.xing.cats.typeclass

import cats.data.OptionT
import cats.effect.IO
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OptionExtendSpec extends AnyWordSpec with Matchers {

  "Greeting" should {

    "get value from Option" in {
      val customGreeting: Option[String] = Some("welcome back, Lola")

      val excitedGreeting: Option[String] = customGreeting.map(_ + "!")
      excitedGreeting.get shouldEqual "welcome back, Lola!"

      val hasWelcome: Option[String] = customGreeting.filter(_.contains("welcome"))
      hasWelcome.get shouldEqual "welcome back, Lola"

      val noWelcome: Option[String] = customGreeting.filterNot(_.contains("welcome"))
      noWelcome shouldEqual None

      val withFallback: String = customGreeting.getOrElse("hello, there!")
      withFallback shouldEqual "welcome back, Lola"

    }

    "get value from IO[Option]" in {
      val customGreeting: IO[Option[String]] = IO(Some("welcome back, Lola"))

      val excitedGreeting: IO[Option[String]] = ???
      excitedGreeting.unsafeRunSync().get shouldEqual "welcome back, Lola!"

      val hasWelcome: IO[Option[String]] = ???
      hasWelcome.unsafeRunSync().get shouldEqual "welcome back, Lola"

      val noWelcome: IO[Option[String]] = ???
      noWelcome.unsafeRunSync() shouldEqual None

      val withFallback: IO[String] = ???
      withFallback.unsafeRunSync() shouldEqual "welcome back, Lola"

    }

    "get value from OptionT" in {
      val customGreeting: IO[Option[String]] = IO(Some("welcome back, Lola"))
      val customGreetingT: OptionT[IO, String] = OptionT(customGreeting)

      val excitedGreeting: OptionT[IO, String] = ???
      excitedGreeting.value.unsafeRunSync().get shouldEqual "welcome back, Lola!"

      val hasWelcome: OptionT[IO, String] = ???
      hasWelcome.value.unsafeRunSync().get shouldEqual "welcome back, Lola"

      val noWelcome: OptionT[IO, String] = ???
      noWelcome.value.unsafeRunSync() shouldEqual None

      val withFallback: IO[String] = ???
      withFallback.unsafeRunSync() shouldEqual "welcome back, Lola"

    }
  }

  "optionT" should {
    "create optionT from option and io" in {
      val greetingFO: IO[Option[String]] = IO(Some("Hello"))

      val firstnameF: IO[String] = IO("Jane")

      val lastnameO: Option[String] = Some("Doe")

      val ot: OptionT[IO, String] = ???

      val result: IO[Option[String]] = ot.value
      result.unsafeRunSync() shouldEqual Some("Hello Jane Doe")

      // what if one of them is none
    }
  }

  "optionT" should {
    val number: IO[Option[Int]] = IO(Some(15))
    val numberT: OptionT[IO, Int] = OptionT(number)

    "map int to string" in {
      def stingCount(number: Int): String = number match {
        case 0 => "have no item"
        case 1 => "have 1 item"
        case n => s"have $n items"
      }

      numberT.map(stingCount).value.unsafeRunSync().get shouldEqual "have 15 items"
    }

    "convert with option" in {

      def stingCountOption(number: Int): Option[String] = number match {
        case 0 => None
        case 1 => Some("have 1 item")
        case n => Some(s"have $n items")
      }

    }

    "convert with IO option" in {
      def stingCountF(number: Int): IO[Option[String]] = number match {
        case 0 => IO(None)
        case 1 => IO(Some("have 1 item"))
        case n => IO(Some(s"have $n items"))
      }

    }

    "convert with OptionT" in {
      def stingCountT(number: Int): OptionT[IO, String] = number match {
        case 0 => OptionT.fromOption(None)
        case 1 => OptionT[IO, String](IO(Some("have 1 item")))
        case n => OptionT[IO, String](IO(Some(s"have $n items")))
      }

    }
  }

}
