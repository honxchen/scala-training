package com.xing.io

import cats.effect.{ContextShift, IO}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.ExecutionContext

class IOTest extends AnyWordSpec with Matchers{
  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  "io" should {
      "convert future to io s" in {
        import cats.effect.{IO, Async}

        import scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.Future

        val apiCall = Future.successful("I come from the Future!")

        val ioa: IO[String] =
          Async[IO].async { cb =>
            import scala.util.{Failure, Success}

            apiCall.onComplete {
              case Success(value) => cb(Right(value))
              case Failure(error) => cb(Left(error))
            }
          }

        println(ioa.unsafeRunSync())
        println(IO.fromFuture(IO(apiCall)).unsafeRunSync())
      }
    }
}
