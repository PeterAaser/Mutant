package scalapagos

import utils._
import IntBonusOps._
import cats._
import cats.implicits._

object Fungus {

  type Cell = Array[Double]


  implicit class gridOps(val grid: Array[Array[Option[Cell]]]) extends AnyVal {
    // wild abuse of nomenclature nonwithstanding
    def flatLift(x: Int, y: Int): Option[Cell] = {
      if (x < 0 || x >= grid.length) None
      else {
        val inner = grid(x)
        if (y <= 0 || y >= inner.length) None
        else inner(y)
      }
    }

    // Do we ever want to call this only on a cells neighbors?
    def foldNeighbours[A](ii: Int)(jj: Int)(fta: Cell => A, empty: A)(ftb: (A, A) => A): A = {
      List(
        (ii - 1, jj + 1), (ii    , jj + 1), (ii + 1, jj + 1),
        (ii - 1, jj    ),                   (ii + 1, jj    ),
        (ii - 1, jj - 1), (ii    , jj - 1), (ii + 1, jj - 1),
      ).map(Function.tupled(flatLift)).flatten.map(fta).foldLeft(empty)(ftb)
    }

    def foreachNeighbours[A](ii: Int)(jj: Int)(fta: Cell => Unit): Unit = {
      List(
        (ii - 1, jj + 1), (ii    , jj + 1), (ii + 1, jj + 1),
        (ii - 1, jj    ),                   (ii + 1, jj    ),
        (ii - 1, jj - 1), (ii    , jj - 1), (ii + 1, jj - 1),
      ).map(Function.tupled(flatLift)).flatten.foreach(fta)
    }
  }


  implicit class CellOps(val cell: Array[Double]) extends AnyVal {
    // macro
    def energy    = cell(1)
    def cytoplasm = cell(2)

    // control
    def SUCC      = cell(0)

    // micro (anything)
  }



  class mold(cells: Array[Array[Option[Cell]]]) {

    // this should be baked into diffusionRateLookup
    lazy val diffusionFactor = hardcode(1.0)
    lazy val osmosisTransfer = hardcode(1.0)

    val diffusionRateLookup: Array[Double] = ???


    // For efficiency we use a recipient cell rather than folding a bunch of cells
    def transferSingle(center: Cell, neighbor: Cell, recipient: Cell): Unit = {
      val pressureDiff = ((neighbor.cytoplasm + center.SUCC) - (center.cytoplasm + neighbor.SUCC))*osmosisTransfer
      for(ii <- 1 until center.size){
        recipient(ii) += neighbor(ii)*pressureDiff
      }
    }


    // Transfer based on osmosis
    def transfer(ii: Int, jj: Int, recipient: Cell): Unit = {
      val cell = cells(ii)(jj).get
      cells.foreachNeighbours(ii)(jj)(transferSingle(cell,_,recipient))
    }


    // Transfer based on diffusion. Does not transfer energy or cytoplasm, doesn't care about SUCC
    def diffuseSingle(center: Cell, neighbor: Cell, recipient: Cell): Unit = {
      for(ii <- 2 until center.size){
        // Diffusionrate and factor can (and will) be prebaked
        recipient(ii) += (neighbor(ii) - center(ii))*diffusionFactor*diffusionRateLookup(ii)
      }
    }


    def diffuse(ii: Int, jj: Int, recipient: Cell): Unit = {
      val cell = cells(ii)(jj).get
      cells.foreachNeighbours(ii)(jj)(diffuseSingle(cell,_,recipient))
    }
  }
}
