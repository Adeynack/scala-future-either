package com.github.adeynack.scala.concurrent

import scala.concurrent.{ExecutionContext, Future}

object FutureOfEither {

  implicit class FutureOfEitherExtensions[A, B](val underlying: Future[Either[A, B]]) {

    /**
      * Maps the [[Right]] value through a function that returns a new [[Right]] value.
      *
      * - Can be changed to a `Left`:   No
      * - Value of `Right` can change:  Yes
      * - Type of `Right` can change:   Yes
      * - Value returned as Future:     No
      */
    def rightMapTo[S](f: B => S)(implicit executor: ExecutionContext): Future[Either[A, S]] = underlying.flatMap {
      case Left(a) => Future.successful(Left[A, S](a))
      case Right(b) => Future(f(b)).map(Right[A, S])
    }

    /**
      * Maps the [[Right]] value through a function that returns a [[Future]] of the new [[Right]] value.
      *
      * - Can be changed to a `Left`:   No
      * - Value of `Right` can change:  Yes
      * - Type of `Right` can change:   Yes
      * - Value returned as Future:     Yes
      *
      */
    def rightFlatMapTo[S](f: B => Future[S])(implicit executor: ExecutionContext): Future[Either[A, S]] = underlying.flatMap {
      case Left(a) => Future.successful(Left[A, S](a))
      case Right(b) => f(b).map(Right[A, S])
    }

    /**
      * Maps the [[Right]] value through a function that returns a new [[Either]] value.
      *
      * - Can be changed to a `Left`:   Yes
      * - Value of `Right` can change:  Yes
      * - Type of `Right` can change:   Yes
      * - Value returned as Future:     No
      */
    def rightMap[S](f: B => Either[A, S])(implicit executor: ExecutionContext): Future[Either[A, S]] = underlying.flatMap {
      case Left(a) => Future.successful(Left[A, S](a))
      case Right(b) => Future(f(b))
    }

    /**
      * Maps the [[Right]] value through a function that returns a [[Future]] of the new [[Either]] value (can
      * potentially change the input [[Right]] value into a [[Left]] if need be).
      *
      * - Can be changed to a `Left`:   Yes
      * - Value of `Right` can change:  Yes
      * - Type of `Right` can change:   Yes
      * - Value returned as Future:     Yes
      *
      */
    def rightFlatMap[S](f: B => Future[Either[A, S]])(implicit executor: ExecutionContext): Future[Either[A, S]] = underlying.flatMap {
      case Left(a) => Future.successful(Left[A, S](a))
      case Right(b) => f(b)
    }

    /**
      * Maps the [[Right]] value through a function that returns a [[Future]] of the new [[Right]] value.
      *
      * - Can be changed to a `Left`:   Yes
      * - Value of `Right` can change:  Yes
      * - Type of `Right` can change:   No
      * - Value returned as Future:     No
      *
      */
    def rightMapPF(f: PartialFunction[B, Either[A, B]])(implicit executor: ExecutionContext): Future[Either[A, B]] = underlying.flatMap {
      case _: Left[A, B] => underlying
      case r @ Right(value) => Future(f.applyOrElse(value, (_: B) => r))
    }

    /**
      * Maps the [[Right]] value through a function that returns a [[Future]] of the new [[Right]] value.
      *
      * - Can be changed to a `Left`:   Yes
      * - Value of `Right` can change:  Yes
      * - Type of `Right` can change:   No
      * - Value returned as Future:     Yes
      *
      */
    def rightFlatMapPF(f: PartialFunction[B, Future[Either[A, B]]])(implicit executor: ExecutionContext): Future[Either[A, B]] = underlying.flatMap {
      case _: Left[A, B] => underlying
      case Right(value) => f.applyOrElse(value, (_: B) => underlying)
    }

  }

}
