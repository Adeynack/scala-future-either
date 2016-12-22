package com.github.adeynack.scala.concurrent

import com.github.adeynack.scala.concurrent.FutureOfEither._
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class FutureOfEitherDemoSpec extends FreeSpec with Matchers {

  private def success[A](value: A): Future[Either[String, A]] = Future.successful(Right[String, A](value))

  "Methods" - {

    "rightMap" - {

      "maps a right value" in {
        val f = success("42").rightMap(_.toInt)
        Await.result(f, 1.minute).right.get shouldEqual 42
      }

      "maps a right value with failure" in {
        class TestException extends Exception
        val f = success("asd").rightMap(_ => throw new TestException)
        intercept[TestException] {
          Await.result(f, 1.minute)
        }
      }

    }

    "rightFlatMap" - {

      "flat maps a right value from another future" in {
        val f = success("42").rightFlatMap(s => Future(s.toInt))
        Await.result(f, 1.minute).right.get shouldEqual 42
      }

      "flat maps a right value from another failing future" in {
        val f = Future.successful(Right[Int, String]("asd")).rightFlatMap(s => Future(s.toInt))
        intercept[NumberFormatException] {
          Await.result(f, 1.minute)
        }
      }

      "flat maps a right value from another future ... but creating that future fails" in {
        class TestException extends Exception
        val f = Future.successful(Right[Int, String]("asd")).rightFlatMap(_ => throw new TestException)
        intercept[TestException] {
          Await.result(f, 1.minute)
        }
      }

      "flat maps a right value from a left value" in {
        val f = Future.successful(Left[Int, String](-1)).rightFlatMap(s => Future(s.toInt))
        Await.result(f, 1.minute).left.get shouldEqual -1
      }

    }

    "rightMapWith" - {

      "maps a right value to another Either" in {
        val f = success("42").rightMapWith(s => Right(Seq(s, "Life", "Universe", "Everything")))
        Await.result(f, 1.minute).right.get shouldEqual Seq("42", "Life", "Universe", "Everything")
      }

      "maps a right value to another Either with failure" in {
        class TestException extends Exception
        val f = success("42").rightMapWith(_ => throw new TestException)
        intercept[TestException] {
          Await.result(f, 1.minute)
        }
      }

      "maps a right value to a left" in {
        val f = success("42").rightMapWith(s => Left("I do not like this value"))
        Await.result(f, 1.minute).left.get shouldEqual "I do not like this value"
      }

    }

    "rightFlatMapWith" - {

      "flat maps a right value to another future of either" in {
        val f = success("42").rightFlatMapWith(s => Future(Right(Seq(s, "Life", "Universe", "Everything"))))
        Await.result(f, 1.minute).right.get shouldEqual Seq("42", "Life", "Universe", "Everything")
      }

      "maps a right value to another failure future of either" in {
        class TestException extends Exception
        val f = success("42").rightFlatMapWith(s => Future.failed(new TestException))
        intercept[TestException] {
          Await.result(f, 1.minute)
        }
      }

      "maps a right value to another future ... but creating that future fails" in {
        class TestException extends Exception
        val f = success("42").rightFlatMapWith(_ => throw new TestException)
        intercept[TestException] {
          Await.result(f, 1.minute)
        }
      }

      "maps a right value to another future of left" in {
        val f = success("42").rightFlatMapWith(s => Future(Left("I do not like this value")))
        Await.result(f, 1.minute).left.get shouldEqual "I do not like this value"
      }

    }

    "rightMapWithPF" - {

      "maps a right value to another value using a partial function that matches and returns another right" in {
        val f = success("42").rightMapWithPF {
          case "42" => Right("Life, Universe, Everything!")
        }
        Await.result(f, 1.minute).right.get shouldEqual "Life, Universe, Everything!"
      }

      "maps a right value to the same value using a partial function that does not match" in {
        val f = success("41").rightMapWithPF {
          case "42" => Right("Life, Universe, Everything!")
        }
        Await.result(f, 1.minute).right.get shouldEqual "41"
      }

      "maps a right value using a partial function that fails" in {
        class TestException extends Exception
        val f = success("42").rightMapWithPF {
          case _ => throw new TestException
        }
        intercept[TestException] {
          Await.result(f, 1.minute)
        }
      }

      "maps a right value to another value using a partial function that matches and returns a left" in {
        val f = success("41").rightMapWithPF {
          case "41" => Left("Not fun...")
        }
        Await.result(f, 1.minute).left.get shouldEqual "Not fun..."
      }

    }

    "rightFlatMapWithPF" - {

      "maps a right value to a future of another value using a partial function that matches and returns another right" in {
        val f = success("42").rightFlatMapWithPF {
          case "42" => Future(Right("Life, Universe, Everything!"))
        }
        Await.result(f, 1.minute).right.get shouldEqual "Life, Universe, Everything!"
      }

      "maps a right value to a future the same value using a partial function that does not match" in {
        val f = success("41").rightFlatMapWithPF {
          case "42" => Future(Right("Life, Universe, Everything!"))
        }
        Await.result(f, 1.minute).right.get shouldEqual "41"
      }

      "maps a right value to a future using a partial function that fails" in {
        class TestException extends Exception
        val f = success("42").rightFlatMapWithPF {
          case _ => Future.failed(new TestException)
        }
        intercept[TestException] {
          Await.result(f, 1.minute)
        }
      }

      "maps a right value to a future using a partial function ... but that future creation fails" in {
        class TestException extends Exception
        val f = success("42").rightFlatMapWithPF {
          case _ => throw new TestException
        }
        intercept[TestException] {
          Await.result(f, 1.minute)
        }
      }

      "maps a right value to a future another value using a partial function that matches and returns a left" in {
        val f = success("41").rightFlatMapWithPF {
          case "41" => Future(Left("Not fun..."))
        }
        Await.result(f, 1.minute).left.get shouldEqual "Not fun..."
      }

    }

  }

  "Use cases" - {

    "Authentication" - {

      //
      // the use-case is in function `authenticate`
      //

      sealed abstract class Result(
        val code: Int
      ) {
        def body: String
      }

      case class OK(body: String) extends Result(200)

      case class Unauthorized(body: String) extends Result(401)

      case class InternalServerError(body: String) extends Result(500)

      case class RawUser(username: String, id: Int)

      case class User(id: String, name: String)

      case class Request(
        queryString: Map[String, Seq[String]]
      ) {

        def getQueryString(key: String): Option[String] = queryString.get(key).flatMap(_.headOption)

      }

      def doSomethingToRequestAndReturn(request: Request) = Future {
        Thread.sleep(100)
        request
      }

      def getUserInfo(request: Request): Future[Either[String, RawUser]] = Future {
        request.getQueryString("user").map {
          case "" => Left("The name was empty")
          case "Chuck" => throw new RuntimeException("Cannot handle Chuck!")
          case username => Right(RawUser(username, 42))
        } getOrElse {
          Left("User name was not part of the query")
        }
      }

      def moreAsyncServiceCall(user: RawUser): Future[RawUser] = Future {
        Thread.sleep(100) // more async stuff
        if (user.username == "LessIsMore") throw new RuntimeException("Less is indeed more, Dave.")
        user
      }

      def authenticate(request: Request): Future[Result] = {
        doSomethingToRequestAndReturn(request)
          .flatMap(getUserInfo)
          //
          // -- rightFlatMapPartial --> From `Right[A,B]` to `Either
          //
          // Can be changed to a `Left` .... Yes
          // Value of `Right` can change ... Yes
          // Type of `Right` can change .... No
          //
          .rightFlatMapWithPF {
            case u @ RawUser("allsolow", _) => Future.successful(Right(u.copy(username = "AllNoLongerSoLow")))
            case RawUser("DIE", _) => Future.successful(Left("I was asked to die :'( buuuuhuuuuu..."))
          }
          //
          // -- rightFlatMap --> from `Right[A,B]` to `Either[A,S]`
          //
          // Can be changed to a `Left` .... Yes
          // Value of `Right` can change ... Yes
          // Type of `Right` can change .... Yes
          //
          // NB: The match has to be COMPLETE, otherwise a MatchException will be thrown.
          //
          .rightFlatMapWith {
            case RawUser("Norris", _) => Future.successful(Left("Cannot handle M. Norris!"))
            case user: RawUser => moreAsyncServiceCall(user).map(Right(_))
          }
          //
          // -- rightMap --> from `Right[A,B]` to `Right[A,S]`
          //
          // Can be changed to a `Left` .... No
          // Value of `Right` can change ... Yes
          // Type of `Right` can change .... Yes
          //
          .rightFlatMap {
            case RawUser(name, id) => Future.successful[User](User(s"ID:$id", name))
          }
          //
          // Normal Scala `flatMap`
          //
          .flatMap {
            case Left(reason: String) => Future.successful(Unauthorized(reason))
            case Right(user) => Future.successful(OK(s"Authenticated as ${user.name}"))
          }
          //
          // Normal Scala `recover`
          //
          .recover {
            case (error) =>
              println(error)
              InternalServerError("Could not authenticate request")
          }

      }

      def authenticateFlatten(request: Request): Future[Result] = {
        doSomethingToRequestAndReturn(request)
          .flatMap(getUserInfo)
          //
          // -- rightFlatMapPartial --> From `Right[A,B]` to `Either
          //
          // Can be changed to a `Left` .... Yes
          // Value of `Right` can change ... Yes
          // Type of `Right` can change .... No
          //
          .rightMapWithPF {
            case u @ RawUser("allsolow", _) => Right(u.copy(username = "AllNoLongerSoLow"))
            case RawUser("DIE", _) => Left("I was asked to die :'( buuuuhuuuuu...")
          }
          //
          // -- rightFlatMap --> from `Right[A,B]` to `Either[A,S]`
          //
          // Can be changed to a `Left` .... Yes
          // Value of `Right` can change ... Yes
          // Type of `Right` can change .... Yes
          //
          // NB: The match has to be COMPLETE, otherwise a MatchException will be thrown.
          //
          .rightFlatMapWith {
            case RawUser("Norris", _) => Future(Left("Cannot handle M. Norris!"))
            case user: RawUser => moreAsyncServiceCall(user).map(Right(_))
          }
          //
          // -- rightMap --> from `Right[A,B]` to `Right[A,S]`
          //
          // Can be changed to a `Left` .... No
          // Value of `Right` can change ... Yes
          // Type of `Right` can change .... Yes
          //
          .rightMap {
            case RawUser(name, id) => User(s"ID:$id", name)
          }
          //
          // Normal Scala `flatMap`
          //
          .map {
            case Left(reason: String) => Unauthorized(reason)
            case Right(user) => OK(s"Authenticated as ${user.name}")
          }
          //
          // Normal Scala `recover`
          //
          .recover {
            case (error) =>
              println(error)
              InternalServerError("Could not authenticate request")
          }

      }

      def authWithName(userQueryParam: String): Result = {
        val request = Request(
          if (userQueryParam == null) Map()
          else Map("user" -> Seq(userQueryParam))
        )
        val result1 = Await.result(authenticate(request), 1.minute)
        val result2 = Await.result(authenticateFlatten(request), 1.minute)
        result1 shouldEqual result2
        result1
      }

      "authenticate when username is passed as query parameter" in {
        authWithName("Joe") shouldEqual OK("Authenticated as Joe")
      }

      "result in a `unauthorized` when name is empty" in {
        authWithName("") shouldEqual Unauthorized("The name was empty")
      }

      "result in a `internal server error` when the name is `Chuck`" in {
        authWithName("Chuck") shouldEqual InternalServerError("Could not authenticate request")
      }

      "result in a `unauthorized` when the name is not provided" in {
        authWithName(null) shouldEqual Unauthorized("User name was not part of the query")
      }

      "result in a `OK` with different user name when the name is `allsolow`" in {
        authWithName("allsolow") shouldEqual OK("Authenticated as AllNoLongerSoLow")
      }

      "result in a `unauthorized` when the name is `DIE`" in {
        authWithName("DIE") shouldEqual Unauthorized("I was asked to die :'( buuuuhuuuuu...")
      }

      "result in a `unauthorized` when the name is `Norris`" in {
        authWithName("Norris") shouldEqual Unauthorized("Cannot handle M. Norris!")
      }

      "result in a `internal server error` when the name is `LessIsMore`" in {
        authWithName("LessIsMore") shouldEqual InternalServerError("Could not authenticate request")
      }

    }

  }

}
