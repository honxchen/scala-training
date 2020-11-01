package com.xing.cats.typeclass

import cats.data.EitherT
import cats.effect.IO
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

class EitherExtendSpec extends AnyWordSpec with Matchers {

  "Division" should {
    def parseDouble(s: String): Either[String, Double] =
      Try(s.toDouble).map(Right(_)).getOrElse(Left(s"$s is not a number"))

    def divide(a: Double, b: Double): Either[String, Double] =
      Either.cond(b != 0, a / b, "Cannot divide by zero")

    def parseDoubleAsync(s: String): IO[Either[String, Double]] = IO(parseDouble(s))

    def divideAsync(a: Double, b: Double): IO[Either[String, Double]] = IO(divide(a, b))


    "get value" in {
      def divisionProgramAsync(inputA: String, inputB: String): IO[Either[String, Double]] =
        parseDoubleAsync(inputA) flatMap { eitherA =>
          parseDoubleAsync(inputB) flatMap { eitherB =>
            (eitherA, eitherB) match {
              case (Right(a), Right(b)) => divideAsync(a, b)
              case (Left(err), _) => IO(Left(err))
              case (_, Left(err)) => IO(Left(err))
            }
          }
        }

      divisionProgramAsync("10", "4").unsafeRunSync() shouldEqual Right(2.5)
      divisionProgramAsync("a", "b").unsafeRunSync() shouldEqual Left("a is not a number")
     }

    "get value with Either T" in {
      def divisionProgramAsync(inputA: String, inputB: String): EitherT[IO, String, Double] =
        ???

      divisionProgramAsync("10", "4").value.unsafeRunSync() shouldEqual Right(2.5)
      divisionProgramAsync("a", "b").value.unsafeRunSync() shouldEqual Left("a is not a number")

    }
  }
}
