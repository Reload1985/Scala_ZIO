package part3concurrency

import zio._
import utils.*

import java.io.File
import java.util.Scanner

object Resources extends ZIOAppDefault {

  // finalizers
  def unsafeMethod(): Int = throw new RuntimeException("Not an Int here for you")
  val anAttempt = ZIO.attempt(unsafeMethod())

  // finalizers
  val attemptWithFinalizer = anAttempt.ensuring(ZIO.succeed("Finalizer").debugThread)

  // multiple finalizers
  val attemptWith2Finalizers = attemptWithFinalizer.ensuring(ZIO.succeed("Another finalizer").debugThread)

  // resource lifecycle

  class Connection(url: String) {
    def open() = ZIO.succeed(s"Opening connection to $url....").debugThread
    def close() = ZIO.succeed(s"CLosing connection to $url !").debugThread
  }

  object Connection {
    def create(url: String) = ZIO.succeed(new Connection(url))
  }

  val fetchUrl = for {
    conn <- Connection.create("rockthejvm.com")
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield() // resource leak

  val corretcFetchUrl = for {
    conn <- Connection.create("rockthejvm.com")
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).ensuring(conn.close()).fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield () // preventing leaks

  // tedious

  // aquireRelease
  val cleanConnection = ZIO.acquireRelease(Connection.create("rockthejvm.com"))(_.close())
  val fetchWithResource = for {
    conn <- cleanConnection
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield () // preventing leaks

  val fetchWithScopedResources = ZIO.scoped(fetchWithResource)

  // aquireReleaseWith
  val cleanConnection_v2 = ZIO.acquireReleaseWith(
    Connection.create("rockthejvm.com") // acquire
  ) (
    _.close() // release
  ) (
    conn => conn.open() *> ZIO.sleep(300.seconds) // use
  )

  val fetchWithResource_v2 = for {
    fib <- cleanConnection_v2.fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield()

  /*
  * Exercises
  * 1. Use the acquireRelease to open a file, print all lines, (one every 100 millis), then close the file
  * */

  def openFileScanner(path: String): UIO[Scanner] =
    ZIO.succeed(new Scanner(new File(path)))

  def readLineByLine(scanner: Scanner): UIO[Unit] =
    if (scanner.hasNextLine)
      ZIO.succeed(scanner.nextLine()).debugThread *> ZIO.sleep(100.millis) *> readLineByLine(scanner)
    else
      ZIO.unit

  def acquireOpenFile(path: String): UIO[Unit] =
    ZIO.succeed(s"opening file at $path...").debugThread *>
      ZIO.acquireReleaseWith(
        openFileScanner(path) // acquire
      ) (
        scanner => ZIO.succeed(s"closing file from $path").debugThread *> ZIO.succeed(scanner.close()) // close
      ) (
        readLineByLine // usage effect
      )

  val testInterruptFileDisplay = for {
    fib <- acquireOpenFile("src/main/scala/part3concurrency/Resources.scala").fork
    _ <- ZIO.sleep(2.seconds) *> fib.interrupt
  } yield()

  // acquireRelease and acquireReleaseWith
  def connFromConfig(path: String): UIO[Unit] =
    ZIO.acquireReleaseWith(openFileScanner(path))(scanner => ZIO.succeed("closing file").debugThread *> ZIO.succeed(scanner.close())) { scanner =>
      ZIO.acquireReleaseWith(Connection.create(scanner.nextLine()))(_.close()){ conn =>
        conn.open() *> ZIO.never
      }
    }

  // nested resource
  def connFromConfig_v2(path: String): UIO[Unit] = ZIO.scoped {
    for {
      scanner <- ZIO.acquireRelease(openFileScanner(path))(scanner => ZIO.succeed("closing file").debugThread *> ZIO.succeed(scanner.close()))
      conn <- ZIO.acquireRelease(Connection.create(scanner.nextLine()))(_.close())
      _ <- conn.open() *> ZIO.never
    } yield()
  }

  def run = connFromConfig_v2("src/main/resources/connection.conf")

}

