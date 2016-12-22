import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object FutureOfEither {

  implicit class FutureOfEitherExtensions[A, B](val underlying: Future[Either[A, B]]) {

    /**
      * Maps the [[Right]] value through a function that returns a [[Future]] of the new [[Right]] value.
      *
      * - Can be changed to a `Left`:  No
      * - Value of `Right` can change: Yes
      * - Type of `Right` can change:  Yes
      *
      */
    def rightMap[S](f: B => Future[S]): Future[Either[A, S]] = underlying.flatMap {
      case Left(a) => Future.successful(Left[A, S](a))
      case Right(b) => f(b).map(Right[A, S])
    }

    /**
      * Maps the [[Right]] value through a function that returns a [[Future]] of the new [[Either]] value (can
      * potentially change the input [[Right]] value into a [[Left]] if need be).
      *
      * - Can be changed to a `Left`:  Yes
      * - Value of `Right` can change: Yes
      * - Type of `Right` can change:  Yes
      *
      */
    def rightFlatMap[S](f: B => Future[Either[A, S]]): Future[Either[A, S]] = underlying.flatMap {
      case Left(a) => Future.successful(Left[A, S](a))
      case Right(b) => f(b)
    }

    /**
      * Maps the [[Right]] value through a function that returns a [[Future]] of the new [[Right]] value.
      *
      * - Can be changed to a `Left`:  Yes
      * - Value of `Right` can change: Yes
      * - Type of `Right` can change:  No
      *
      */
    def rightFlatMapPartial(f: PartialFunction[B, Future[Either[A, B]]]): Future[Either[A, B]] = underlying.flatMap {
      case left: Left[A, B] => Future.successful(left)
      case right @ Right(b) =>
        if (f.isDefinedAt(b)) f(b)
        else Future.successful(right)
    }

  }

}
