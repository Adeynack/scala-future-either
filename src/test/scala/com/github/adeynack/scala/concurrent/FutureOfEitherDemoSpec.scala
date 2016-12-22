package com.github.adeynack.scala.concurrent

import com.github.adeynack.scala.concurrent.FutureOfEither._
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class FutureOfEitherDemoSpec extends FreeSpec with Matchers {

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
          .rightMap {
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

      def authWithName(userQueryParam: String): Result = {
        val futureResult = authenticate(Request(
          if (userQueryParam == null) Map()
          else Map("user" -> Seq(userQueryParam))
        ))
        Await.result(futureResult, 1.minute)
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
