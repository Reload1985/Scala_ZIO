package part3concurrency

import zio._
import utils._

object Parallelism extends ZIOAppDefault {

  val meaningOfLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("Scala")
  val combined = meaningOfLife.zip(favLang) // combines/zip are sequential

  // combine in parallel
  val combinedPar = meaningOfLife.zipPar(favLang) // combination is parallel

  /*
  * start each on fiber
  * what if one fails? the other should be interrupted
  * what if one is interrupted? the entire thing should be interrupted
  * what if the whole thing is interrupted? need to interrupt both effects
  * */

  // try a zipPar combinator
  // hint: fork/join/await, interrupt

  def myZipPar[R,E,A,B](zioa: ZIO[R,E,A], ziob: ZIO[R,E,B]): ZIO[R,E,(A,B)] = {
    val exits = for {
      fiba <- zioa.fork
      fibb <- ziob.fork
      exita <- fiba.await
      exitb <- exita match {
          case Exit.Success(value) => fibb.await
          case Exit.Failure(_) => fibb.interrupt
        }
    } yield(exita, exitb)
    exits.flatMap {
      case (Exit.Success(a), Exit.Success(b)) => ZIO.succeed((a,b))
      case (Exit.Success(_), Exit.Failure(cause)) => ZIO.failCause(cause)
      case (Exit.Failure(cause), Exit.Success(_)) => ZIO.failCause(cause)
      case (Exit.Failure(c1), Exit.Failure(c2)) => ZIO.failCause(c1 && c2)
    }
  }

  // parallel combinators
  // zipPar, zipWithPar

  // collectAllPar
  val effects: Seq[ZIO[Any, Nothing, Int]] = (1 to 10).map(i => ZIO.succeed(i).debugThread)
  val collectedValues: ZIO[Any, Nothing, Seq[Int]] = ZIO.collectAllPar(effects) // "traverse"

  // foreachPar
  val printlnParallel = ZIO.foreachPar((1 to 10).toList)(i => ZIO.succeed(println(i)))

  // reduceAllPar, mergeAllPar
  val sumPar = ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _)
  val sumPar_v2 = ZIO.mergeAllPar(effects)(0)(_ + _)

  /*
  * if all the effects succeed, all good
  * one effects fail => everyone else is interrupted, error is surfaced
  * one effect is interrupted => everyone else is interrupted, error = interruption (for the big expression)
  * if the entire thing is interrupted => all effects are interrupted
  * */

  /*
  * Exercise: word counting exercise from "Fibers", using parallelism combinator
  * */

  def countWords(path: String): UIO[Int] =
    ZIO.succeed {
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString("").split(" ").count(_.nonEmpty)
      println(s"Counted $nWords in $path")
      source.close()
      nWords
    }

  def wordCountParallel(n: Int): UIO[Int] = {
    val effects = (1 to n)
      .map(i => s"src/main/resources/textfile_$i.txt")
      .map(path => countWords(path))

    // v1 - collectAllPar()
    ZIO.collectAllPar(effects).map(_.sum)

    // v2 - mergeAllPar()
    ZIO.mergeAllPar(effects)(0)(_ + _)

    // v3 - reduceAllPar
    ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _)
  }

  def run = wordCountParallel(10).debugThread
}
