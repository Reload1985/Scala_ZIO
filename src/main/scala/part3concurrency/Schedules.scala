package part3concurrency

import zio._
import utils._

object Schedules extends ZIOAppDefault {

  val aZIO = Random.nextBoolean.flatMap{
    flag =>
      if (flag) ZIO.succeed("fetched value!").debugThread
      else ZIO.succeed("failure....").debugThread *> ZIO.fail("error")
  }

  val aRetriedZIO = aZIO.retry(Schedule.recurs(10)) // retries 10 times, returns the first success, last failure

  // schedules are data structures that describes HOW effects should be timed
  val oneTimeSchedule = Schedule.once
  val recurrentSchedule = Schedule.recurs(10)
  val fixedIntervalSchedule = Schedule.spaced(1.second) // retries every 1sec until a success is returned

  // exponential backoff
  val exBackoffSchedule = Schedule.exponential(1.second, 2.0)
  val fiboSchedule = Schedule.fibonacci(1.second) // 1s, 1s, 2s, 3s, 5s, ...

  // combinators
  val recurrentAndSpaced = Schedule.recurs(3) && Schedule.spaced(1.second) // every attempt is 1s apart, 3 attempts total

  // sequencing
  val recurrentThenSpaced = Schedule.recurs(3) ++ Schedule.spaced(1.second) // 3 retries, then every 1s

  /*
  * Schedules have:
    * R = enviroment
    * I = input (errors in the case of .retry, values in the case of .repeat)
    * O = output (values for the next schedule so that you can do something with them)
  * */

  val totalElapsed = Schedule.spaced(1.second) >>> Schedule.elapsed.map(time => println(s"total time elapsed: $time"))

  def run = aZIO.retry(totalElapsed)

  /*
  * Result:
  [ZScheduler-Worker-3] failure....
  total time elapsed: PT0S
  [ZScheduler-Worker-5] failure....
  total time elapsed: PT1.0268376S
  [ZScheduler-Worker-1] failure....
  total time elapsed: PT2.0342783S
  [ZScheduler-Worker-4] fetched value!

  Process finished with exit code 0
  * */
}
