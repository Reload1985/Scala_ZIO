package part2effects

import zio.*

import java.net.NoRouteToHostException
import scala.sys.process.processInternal.IOException
import scala.util.{Failure, Success, Try}

object ZIOErrorHandling extends ZIOAppDefault {

  //ZIOs can fail
  val aFailedZio = ZIO.fail("Somethign went wrong")
  val failedWithThrowable = ZIO.fail(new RuntimeException("Boom!"))
  val failedWithDescription = failedWithThrowable.mapError(_.getMessage)

  // attempt: run a effect that might throw an exception
  val badZIO = ZIO.succeed{
    println("Trying something")
    val string: String = null
    string.length
  } // this is bad

  val anAttempt: ZIO[Any, Throwable, Int] = ZIO.attempt {
    println("Trying something")
    val string: String = null
    string.length
  }

  // effectfully catch erros
  val catchError = anAttempt.catchAll(e => ZIO.attempt(s"Returning a diferent value beacause $e"))
  val catchSelectiveErrors = anAttempt.catchSome{
    case e: RuntimeException => ZIO.succeed(s"Ignoring runtime exceptions: $e")
    case _ => ZIO.succeed("Ignoring everything else")
  }

  // chain effects
  val aBetterAttempt = anAttempt.orElse(ZIO.succeed(56))

  // fold: handle both success and failure
  val handleBoth: ZIO[Any, Nothing, String] = anAttempt.fold(ex => s"Something bad happened: $ex", value => s"length of the string was $value")

  //effectful fold: foldZIO
  val handleBoth_v2 = anAttempt.foldZIO(
    ex => ZIO.succeed(s"Something bad happened: $ex"),
    value => ZIO.succeed(s"Length of the string was $value")
  )

  /*
  * Conversions between Option/Try/Either to ZIO
  * */

  val aTryToZIO: ZIO[Any, Throwable, Int] = ZIO.fromTry(Try(42/0)) // can fail with Throwable

  // either -> ZIO
  val anEither: Either[Int, String] = Right("Success!")
  val anEitherToZIO: ZIO[Any, Int, String] = ZIO.fromEither(anEither)

  // ZIO -> ZIO with Either as the value channel
  val eitherZIO = anAttempt.either

  // reverse
  val anAttempt_v2 = eitherZIO.absolve

  // option -> ZIO
  val anOption: ZIO[Any, Option[Nothing], Int] = ZIO.fromOption(Some(42))

  /*
  * Exercise: implement a version of fromTry, fromOption, fromEither, either, absolve
  * using fold and foldZIO
  * */

  def try2ZIO[A](aTry: Try[A]): Task[A] = aTry match {
    case Failure(exception) => ZIO.fail(exception)
    case Success(value) => ZIO.succeed(value)
  }

  def either2ZIO[A, B](anEither: Either[A, B]): ZIO[Any, A, B] = anEither match {
    case Left(value) => ZIO.fail(value)
    case Right(value) => ZIO.succeed(value)
  }

  def option2ZIO[A](anOption: Option[A]): ZIO[Any, Option[Nothing], A] = anOption match {
    case Some(value) => ZIO.succeed(value)
    case None => ZIO.fail(None)
  }

  def zio2zioEither[R, A, B](zio: ZIO[R, A, B]): ZIO[R, Nothing, Either[A, B]] = zio.foldZIO(
    error => ZIO.succeed(Left(error)),
    value => ZIO.succeed(Right(value))
  )

  def absolveZIO[R,A,B](zio: ZIO[R, Nothing, Either[A,B]]): ZIO[R,A,B] = zio.flatMap {
    case Left(e) => ZIO.fail(e)
    case Right(value) => ZIO.succeed(value)
  }

  /*
  * Errors = failures present in the ZIO type signature ("checked" exception)
  * Defects = failures that are unrecoverable, unforeseen, NOT present in the ZIO type signature

  ZIO[R,E,A] can finish with Exit[E,A]
    - Success[A] containing a value
    - Cause[E]
      - Fail[E] containing the error
      - Die(t: Throwable) which was unforeseen
  * */

  val divisionByZero: UIO[Int] = ZIO.succeed(1/0)

  val failedInt: ZIO[Any, String, Int] = ZIO.fail("I failed")
  val failureCauseExposed: ZIO[Any, Cause[String], Int] = failedInt.sandbox
  val failureCauseHidden: ZIO[Any, String, Int] = failureCauseExposed.unsandbox

  // fold with cause
  val foldedWithCause = failedInt.foldCause(
    cause => s"this failed with ${cause.defects}",
    value => s"this succeeded with $value"
  )

  val foldedWithCause_v2 = failedInt.foldCauseZIO(
    cause => ZIO.succeed(s"this failed with ${cause.defects}"),
    value => ZIO.succeed(s"this succeeded with $value")
  )


  /*
  * Good practice
  - at a lower level, your "errors" should be treated
  - at a higher level, you should hide "errors" and assume they are unrecoverable
  * */

  def callHTTPendpoint(url: String): ZIO[Any, IOException, String] =
    ZIO.fail(new IOException("no internet, dumbass!"))

  val endpointCallWithDefects: ZIO[Any, Nothing, String] =
    callHTTPendpoint("rockthejvm.com").orDie // all errors are now defects

  // redefining the error channel
  def callHTTPEndpointWideError(url: String): ZIO[Any, Exception, String] =
    ZIO.fail(new IOException("NO INTERNET!!!"))

  def callHTTPEndpoint_v2(url: String): ZIO[Any, IOException, String] =
    callHTTPEndpointWideError(url).refineOrDie[IOException] {
      case e: IOException => e
      case _: NoRouteToHostException => new IOException(s"No route to host to $url, can't fetch page")
    }

  // reverse: turn defects into the error channel

  val endpointCallWithError = endpointCallWithDefects.unrefine {
    case e => e.getMessage
  }

  /*
  * Combine effects with different errors
  * */

  case class IndexError(message: String)
  case class DbError(message: String)
  val callAPI: ZIO[Any, IndexError, String] = ZIO.succeed("page: <html></html>")
  val queryDB: ZIO[Any, DbError, Int] = ZIO.succeed(1)
  val combined = for {
    page <- callAPI
    rowsAffected <- queryDB
  } yield (page, rowsAffected) //lost type safety => Product & Serializable (Mouse over)

  /*
  * Solutions
    - design a error model
    - use scala 3 union types
    - .mapError to some common error type
  * */

  /*
  * Exercises
  * */

  //  1  - make this effect fail with typed error
  val aBadFailure = ZIO.succeed[Int(throw new RuntimeException("this is bad!"))
  val aBetterFailure = aBadFailure.sandbox // exposes the defect in the cause
  val aBetterFailure_v2 = aBadFailure.unrefine { // surfaces out the exceptions in the error channel
    case e => e
  }

  //  2 - transform a zio into another zio with a narrower exception type
  def ioException[R,A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] = ???

  //  3
  def left[R,E,A,B](zio: ZIO[R,E, Either[A,B]]): ZIO[R, Either[E,A], B] = ???

  //  4
  val database = Map(
    "daniel" -> 123,
    "alice" -> 789
  )

  case class QueryError(raeson: String)
  case class UserProfile(name: String, phone: Int)

  def lookupProfile(userId: String): ZIO[Any, QueryError, Option[UserProfile]] =
    if(userId != userId.toLowerCase())
      ZIO.fail(QueryError("User ID format is invalid"))
    else
      ZIO.succeed(database.get(userId).map(phone => UserProfile(userId, phone)))

  // surface out all the failed cases of this API

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ???
}
