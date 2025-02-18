package part2effects

import part2effects.ZIOEffects.asUnit
import zio.*

import scala.io.StdIn

object ZIOEffects {

  // success
  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)

  // failure
  val aFailure: ZIO[Any, String, Nothing] = ZIO.fail("Something went wrong")

  // suspension/delay
  val aSuspendedZIO: ZIO[Any, Throwable, Int] = ZIO.suspend(meaningOfLife)

  // map + flatmap
  val improvedMOL = meaningOfLife.map(_ * 2)
  val printingMOL = meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))

  //for-comprehensions
  val smallProgram = for {
    _ <- ZIO.succeed(println("whats your name?"))
    name <- ZIO.succeed(StdIn.readLine())
    _ <- ZIO.succeed(println(s"welcome to ZIO, $name"))
  } yield ()

  // A LOT of combinators
  // zip, zipWith

  val anotherMOL = ZIO.succeed(100)
  val tupledZIO = meaningOfLife.zip(anotherMOL)
  val combinedZIO = meaningOfLife.zipWith(anotherMOL)(_ * _)

  /*
  * Type alaiases of Zios
  * */

  // UIO[A] = ZIO[Any, Nothing, A] - no requirements, cannot fail, produces A
  val aUIO: UIO[Int] = ZIO.succeed(99)

  // URIO[R, A] = ZIO[R, Nothing, A] - can not fail
  val aURIO: URIO[Int, Int] = ZIO.succeed(67)

  // RIO[R, A] = ZIO[R, Throwable, A] - can fail with a throwable
  val anRIO: RIO[Int, Int] = ZIO.succeed(98)
  val aFailedRio: RIO[Int, Int] = ZIO.fail(new RuntimeException("RIO failed"))

  // Task[A] = ZIO[Any, Throwble, A] - no requirements, can fail with a throwable, produces A
  val aSuccessfulTask: Task[Int] = ZIO.succeed(89)
  val aFailedTask: Task[Int] = ZIO.fail(new RuntimeException("Something went bad"))

  //IO[E, A] = ZIO[Any, E, A] - no requirements
  val aSuccessfulIO: IO[String, Int] = ZIO.succeed(34)
  val aFailedIO: IO[String, Int] = ZIO.fail("Something bad happened")


  /*
  * Exercises
  * */

  // 1 - sequence two ZIOs and take the value of the last one
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    zioa.flatMap(a => ziob.map(b => b))

  def sequenceTakeLast_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    for {
      _ <- zioa
      b <- ziob
    } yield b

  // built-in ZIO
  def sequenceTakeLast_v3[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    zioa *> ziob

  // 2 -  sequence two ZIOs and take the value of the first one
  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    zioa.flatMap(a => ziob.map(_ => a))

  // built-in ZIO
  def sequenceTakeFirst_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    zioa <* ziob

  // 3 - run ZIO forever
  def runForever[R,E,A](zio: ZIO[R, E, A]): ZIO[R,E,A] =
    zio.flatMap(_ => runForever(zio))

  def runForever_v2[R,E,A](zio: ZIO[R, E, A]): ZIO[R,E,A] =
    zio *> runForever(zio)

  val endlessLoop = runForever {
    ZIO.succeed{
      println("running...")
      Thread.sleep(1000)
    }
  }

  // 4 - convert the value of a ZIO to something else
  def convert[R,E,A,B](zio: ZIO[R,E,A], value: B):ZIO[R,E,B] =
    zio.map(_ => value)
  def convert_v2[R,E,A,B](zio: ZIO[R,E,A], value: B):ZIO[R,E,B] =
    zio.as(value) // same

  // 5 - Discard the value of a ZIO to Unit
  def asUnit[R,E,A](zio: ZIO[R,E,A]): ZIO[R,E,Unit] =
    convert(zio, ())

  def asUnit_v2[R,E,A](zio: ZIO[R,E,A]): ZIO[R,E,Unit] =
    zio.unit // same


  // 6 - Recursion
  def sum(n: Int): Int =
    if(n == 0) 0
    else n + sum(n - 1) // will crash at sum(20 000)

  def sumZIO(n: Int): UIO[Int] =
    if(n == 0) ZIO.succeed(0)
    else for {
      current <- ZIO.succeed(n)
      prevSum <- sumZIO(n - 1)
    } yield current + prevSum


  // 7 - fibonacci
  // hint: use ZIO.suspend / ZIO.suspendSucceed
  def fibo(n: Int): BigInt =
    if(n <= 2) 1
    else fibo(n - 1) + fibo(n - 2)

  def fiboZIO(n: Int): UIO[BigInt] =
    if(n <= 2 ) 1
    else for {
      last <- fiboZIO(n - 1)
      prev <- fiboZIO(n - 2)
    } yield last + prev

  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default
    implicit val trace: Trace = Trace.empty
    Unsafe.unsafe{
      implicit u =>
        val firstEffect = ZIO.succeed{
          println("computing first effect........")
          Thread.sleep(1000)
          1
        }
        val secondEffect = ZIO.succeed {
          println("computing second effect........")
          Thread.sleep(1000)
          2
        }
        println(runtime.unsafe.run(endlessLoop))
    }
  }
}
