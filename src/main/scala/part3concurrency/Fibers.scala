package part3concurrency

import zio.*
import utils.*

import java.io.{File, FileWriter}

object Fibers extends ZIOAppDefault {

  val meaningOfLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("scala")

  // fiber = lightweight thread

  def createFiber: Fiber[Throwable, String] = ??? // impossible to create manually

  val combinator = for {
    mol <- meaningOfLife.debugThread
    lang <- favLang.debugThread
  } yield(mol, lang)

  val differentThreadIO = for {
    _ <- meaningOfLife.debugThread.fork
    _ <- favLang.debugThread.fork
  } yield()

  val meaningOfLifeFiber: ZIO[Any, Nothing, Fiber[Throwable, Int]] = meaningOfLife.fork

  // join a fiber
  def runOnAnotherThread[R,E,A](zio: ZIO[R,E,A]) = for {
    fib <- zio.fork
    result <- fib.join
  } yield result

  // awaiting a fiber
  def runOnAnotherThread_v2[R,E,A](zio: ZIO[R,E,A]) = for {
    fib <- zio.fork
    result <- fib.await
  } yield result match {
    case Exit.Success(value) => s"succeed with $value"
    case Exit.Failure(cause) => s"failed with $cause"
  }

  // poll - peak at the result of the fiber RIGHT NOW, without blocking

  val peaekFiber = for {
    fib <- ZIO.attempt {
      Thread.sleep(1000)
      42
    }.fork
    result <- fib.poll
  }yield result

  // compose fibers
  // zip
  val zippedFibers = for {
    fib1 <- ZIO.succeed("Result from fiber 1").debugThread.fork
    fib2 <- ZIO.succeed("Result from fiber 2").debugThread.fork
    fiber = fib1.zip(fib2)
    tuple <- fiber.join
  } yield tuple

  // orElse
  val chainedFibers = for {
    fib1 <- ZIO.fail("no good").debugThread.fork
    fib2 <- ZIO.succeed("Result from fiber 2").debugThread.fork
    fiber = fib1.orElse(fib2)
    message <- fiber.join
  } yield message


  /*
  * Exercises
  * */

  // 1 - zip two fibers without using the zip combinator
  def zipFibers[E,A,B](fiber1: Fiber[E,A], fiber2: Fiber[E,B]): ZIO[Any, Nothing, Fiber[E, (A,B)]] = {
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield(v1, v2)
    finalEffect.fork
  }

  val zippedFibers_v2 = for {
    fib1 <- ZIO.succeed("Result from fiber 1").debugThread.fork
    fib2 <- ZIO.succeed("Result from fiber 2").debugThread.fork
    fiber <- zipFibers(fib1, fib2)
    tuple <- fiber.join
  } yield tuple

  def zipFibersGeneral[E, E1 <: E, E2 <: E, A, B](fiber1: Fiber[E1, A], fiber2: Fiber[E2, B]): ZIO[Any, Nothing, Fiber[E, (A, B)]] = {
    //same implementation
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)
    finalEffect.fork
  }


  // 2 - samething with orElse
  def chainFibers[E,A](fiber1: Fiber[E,A], fiber2: Fiber[E,A]): ZIO[Any, Nothing, Fiber[E,A]] =
    fiber1.join.orElse(fiber2.join).fork

  // 3 - distributing a task in between any fibers
  // spawn n fibers, count the n of words in each file,
  // then aggregate all the results together in one big number
  def generateRandomFile(path: String): Unit = {
    val random = scala.util.Random
    val chars = 'a' to 'z'
    val nWords = random.nextInt(2000) // at most 2000 random words

    val content = (1 to nWords)
      .map(_ => (1 to random.nextInt(10)).map(_ => chars(random.nextInt(26))).mkString) // one word for every 1 to nWords
      .mkString(" ")

    val writer = new FileWriter(new File(path))
    writer.write(content)
    writer.flush()
    writer.close()
  }

  // part 1 - an effect that reads one file and counts the words in there
  def countWords(path: String): UIO[Int] =
    ZIO.succeed{
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString("").split(" ").count(_.nonEmpty)
      println(s"Counted $nWords in $path")
      source.close()
      nWords
    }


    // part 2 - spin up fibers for all the files
  def wordCountParallel(n: Int): UIO[Int] = {
    val effects: Seq[ZIO[Any, Nothing, Int]] = (1 to n)
      .map(i => s"src/main/resources/textfile_$i.txt") // paths
      .map(countWords) // list of effects
      .map(_.fork) // list of effects returning fibers
      .map((fiberEff: ZIO[Any, Nothing, Fiber[Nothing, Int]]) => fiberEff.flatMap(_.join)) // list of effects return values (count of words)
    effects.reduce { (zioa, ziob) =>
      for {
       a <- zioa
       b <- ziob
      } yield a + b
    }
  }


  // ZIO.succeed((1 to 10).foreach(i => generateRandomFile(s"src/main/resources/textfile_$i.txt"))) => generates 10 files with random words

  def run = wordCountParallel(10).debugThread

}
