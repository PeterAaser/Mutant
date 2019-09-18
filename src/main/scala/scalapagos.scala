package scalapagos

import utils._

object main {
  def main(args: Array[String]): Unit = {
    say("hehe")
    say(theThing.readerProgram)
    say(theThing.main2.unsafeRunSync)
    say(theThing.main.unsafeRunSync)
  }
}
