
package scalapagos

import utils._
import PrintUtils._

object main {
  def main(args: Array[String]): Unit = {

    // ShittyTest.test

    say("\n\n\n\n\n\n\n\n\n\n")
    say("\n\n\n\n\n\n\n\n\n\n")
    say("\n\n\n\n\n\n\n\n\n\n")
    val dur = Fungus.Mold(30)
    say(printFood(dur.food))
    for(ii <- 0 until 110){
      say(printSlimeMold(dur.cells))
      dur.stepFull
    }
  }
}
