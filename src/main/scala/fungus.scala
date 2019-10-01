package scalapagos

import utils._
import IntBonusOps._
import cats._
import cats.implicits._
import collection.mutable.ArrayBuffer

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

    // utility
    def clear: Unit = for(ii <- 0 until cell.size){cell(ii) = 0.0}
  }



  abstract class Mold {

    // this should be baked into diffusionRateLookup
    lazy val diffusionFactor = hardcode(0.5)
    lazy val osmosisTransfer = hardcode(0.2)

    val diffusionRates: Array[Double] = List.fill(10)(diffusionFactor).toArray

    var liveCells: ArrayBuffer[(Int, Int)]

    var cells:  Array[Array[Option[Cell]]]
    var cellsB: Array[Array[Option[Cell]]]

    // For efficiency we use a recipient cell rather than folding a bunch of cells
    def transferSingle(center: Cell, neighbor: Cell, recipient: Cell): Unit = {
      val cytoTransfer = ((neighbor.cytoplasm + center.SUCC) - (center.cytoplasm + neighbor.SUCC))*osmosisTransfer

      // Move cytoplasm
      recipient(2) += cytoTransfer

      // Recalculate concentrations
      if(cytoTransfer > 0.0){
        val oldCyto = center.cytoplasm/(center.cytoplasm + neighbor.cytoplasm)
        val newCyto = 1.0 - oldCyto
        for(ii <- 3 until center.size){
          recipient(ii) = recipient(ii)*oldCyto + neighbor(ii)*newCyto
        }
      }
    }


    // Transfer based on osmosis
    def transfer(ii: Int, jj: Int, recipient: Cell): Unit = {
      val cell = cells(ii)(jj).get
      cells.foreachNeighbours(ii)(jj)(transferSingle(cell,_,recipient)) 
    }


    // Transfer based on diffusion. Does not transfer energy or cytoplasm, doesn't care about SUCC
    def diffuseSingle(center: Cell, neighbor: Cell, recipient: Cell): Unit = {

      val centerCyto   = center.cytoplasm/(center.cytoplasm + neighbor.cytoplasm)
      val neighborCyto = 1.0 - centerCyto

      for(ii <- 3 until center.size){
        // Diffusionrate and factor can (and will) be prebaked
        val loss = center(ii)*neighborCyto*diffusionRates(ii) 
        val gain = neighbor(ii)*centerCyto*diffusionRates(ii)
        recipient(ii) = center(ii) + gain - loss
      }
    }


    def diffuse(ii: Int, jj: Int, recipient: Cell): Unit = {
      val cell = cells(ii)(jj).get
      cells.foreachNeighbours(ii)(jj)(diffuseSingle(cell,_,recipient))
    }


    // This is order dependent, I don't think that's a big deal
    def stepOsmosis: Unit = liveCells.foreach{ case(ii, jj) =>
      val recipient = cellsB(ii)(jj).get
      for(kk <- 0 until recipient.size){ recipient(kk) = cells(ii)(jj).get(kk) }
      transfer(ii, jj, recipient)
      diffuse(ii, jj, recipient)
    }


    def stepFull: Unit = {
      stepOsmosis
      var tmp = cells
      cells   = cellsB
      cellsB  = tmp
    }

    def populateLiveCells: Unit =
      for(ii <- 0 until cells.size){
        for(jj <- 0 until cells.size){
          if(cells(ii)(jj).isDefined) liveCells.append((ii, jj))
        }
      }
  }
  object Mold {

    def apply(dim: Int): Mold = {
      val ded: Option[Cell] = None
      val m = new Mold {
        override var cells     = List.fill(dim)(List.fill(dim)(ded).toArray).toArray
        override var cellsB    = List.fill(dim)(List.fill(dim)(ded).toArray).toArray
        override var liveCells = new ArrayBuffer[(Int, Int)]()
      }

      //                          SUCC, energy, cyto, signals
      m.cells(2)(5) =  Some(Array(0.0,  1.0,    1.0,  2.0, 1.0, 1.0))
      m.cells(3)(5) =  Some(Array(0.0,  1.0,    1.0,  0.0, 1.0, 1.0))
      m.cells(4)(5) =  Some(Array(0.0,  1.0,    1.0,  0.0, 1.0, 1.0))
      m.cells(5)(5) =  Some(Array(0.0,  1.0,    1.0,  0.0, 1.0, 1.0))
      m.cells(6)(5) =  Some(Array(0.0,  1.0,    1.0,  0.0, 1.0, 1.0))
      m.cells(7)(5) =  Some(Array(0.0,  1.0,    1.0,  5.0, 1.0, 1.0))
      m.cellsB(2)(5) = Some(Array(0.0,  1.0,    0.0,  0.0, 1.0, 1.0))
      m.cellsB(3)(5) = Some(Array(0.0,  1.0,    0.0,  0.0, 1.0, 1.0))
      m.cellsB(4)(5) = Some(Array(0.0,  1.0,    0.0,  0.0, 1.0, 1.0))
      m.cellsB(5)(5) = Some(Array(0.0,  1.0,    0.0,  0.0, 1.0, 1.0))
      m.cellsB(6)(5) = Some(Array(0.0,  1.0,    0.0,  0.0, 1.0, 1.0))
      m.cellsB(7)(5) = Some(Array(0.0,  1.0,    0.5,  0.0, 1.0, 1.0))

      m.populateLiveCells
      m
    }
  }
}
