package scalapagos

import utils._
import PrintUtils._

object main {
  def main(args: Array[String]): Unit = {

    // ShittyTest.test

    val dur = Fungus.Mold(30)
    say("\n\n\n\n\n\n\n\n\n\n")
    say("\n\n\n\n\n\n\n\n\n\n")
    say("\n\n\n\n\n\n\n\n\n\n")
    for(ii <- 0 until 10){
      say(printSlimeMoldSignals(dur.cells))
      dur.stepFull
    }
       
  }
}
