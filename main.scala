
package scalapagos

import utils._
import PrintUtils._

object main {
  def main(args: Array[String]): Unit = {

    say("\n\n\n\n\n\n\n\n\n\n")
    say("\n\n\n\n\n\n\n\n\n\n")
    say("\n\n\n\n\n\n\n\n\n\n")

    GA.gogo
    // say(GA.initGenomes.takeRight(20))
    // val dur = Fungus.Mold(Params(), CellControl.random(100))
    // say(printFood(dur.food))
    // for(ii <- 0 until 10){
    //   say(printPetri(dur.cells, dur.food))
    //   dur.stepFull
    // }
  }
}
