package scalapagos

import utils._

object main {
  def main(args: Array[String]): Unit = {

    ShittyTest.test

    val hurr = (0 to 10).toArray
    say(hurr.slice(0, 3).toList)
    say(hurr.slice(3, 6).toList)
    say(hurr.slice(6, 9).toList)
    say(hurr.slice(9, 12).toList)
    say(hurr.takeRight(3).toList)
  }
}
