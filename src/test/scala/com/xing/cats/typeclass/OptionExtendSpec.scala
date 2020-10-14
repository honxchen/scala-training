package com.xing.cats.typeclass

import cats.data.OptionT
import cats.effect.IO
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OptionExtendSpec extends AnyWordSpec with Matchers {

  "optionT" should {
    "reduce map boilerplate" in {
      import scala.concurrent.ExecutionContext.Implicits.global

      val customGreeting: IO[Option[String]] = IO(Some("welcome back, Lola"))
//      val excitedGreeting: IO[Option[String]] = customGreeting.map(_.map(_ + "!"))
//
//      val hasWelcome: IO[Option[String]] = customGreeting.map(_.filter(_.contains("welcome")))
//
//      val noWelcome: IO[Option[String]] = customGreeting.map(_.filterNot(_.contains("welcome")))
//
//      val withFallback: IO[String] = customGreeting.map(_.getOrElse("hello, there!"))

      val customGreetingT: OptionT[IO, String] = OptionT(customGreeting)

      val excitedGreeting: OptionT[IO, String] = customGreetingT.map(_ + "!")

      val withWelcome: OptionT[IO, String] = customGreetingT.filter(_.contains("welcome"))

      val noWelcome: OptionT[IO, String] = customGreetingT.filterNot(_.contains("welcome"))

      val withFallback: IO[String] = customGreetingT.getOrElse("hello, there!")

      // optionT reduce the io map
      // goto optionT map func, it only call io map
      // find the implicit functor and the
    }

    "create optionT from option and io" in {
      val greetingFO: IO[Option[String]] = IO(Some("Hello"))

      val firstnameF: IO[String] = IO("Jane")

      val lastnameO: Option[String] = Some("Doe")

      val ot: OptionT[IO, String] = for {
        g <- OptionT(greetingFO)
        f <- OptionT.liftF(firstnameF)
        l <- OptionT.fromOption[IO](lastnameO)
      } yield s"$g $f $l"

      val result: IO[Option[String]] = ot.value
      result.unsafeRunSync() shouldEqual Some("Hello Jane Doe")
    }

    "get defualt value if option is null" in {
      val customGreeting: IO[Option[String]] = IO(None)

      val defaultGreeting: IO[String] = IO("hello, there")


      OptionT(customGreeting).getOrElse("hello, there").unsafeRunSync() shouldEqual "hello, there"
      OptionT(customGreeting).getOrElseF(defaultGreeting).unsafeRunSync() shouldEqual "hello, there"
    }
  }
}
