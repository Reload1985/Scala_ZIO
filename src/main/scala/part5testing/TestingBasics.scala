package part5testing

import zio._
import zio.test._

case class Person(name: String, age: Int) {
  def spellName: String = name.toUpperCase()
  def saySomething: UIO[String] = ZIO.succeed(s"Hi, I'm $name")
}

object MyTestSpec extends ZIOSpecDefault {

  // fundamental method
  def spec = test("First Test"){
    val person = Person("Daniel", 99)
    
    // an assertion
    assert(person.spellName)(Assertion.equalTo("DANIEL"))
    // same
    assertTrue(person.spellName == "DANIEL")
  }

}