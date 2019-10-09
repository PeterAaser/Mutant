package scalapagos

import Console.{ printf => _ }

import utils._
import IntBonusOps._
import DoubleBonusOps._
import cats._
import cats.implicits._
import collection.mutable.ArrayBuffer

object Fungus {

  type Cell = Array[Double]

  object Cell {
    val CONTROL = 0
    val ENERGY  = 1
    val CYTO    = 2
    val SUCC    = 3
    val SIGNAL  = 4
  }

  implicit class gridOps(private val grid: Array[Array[Option[Cell]]]) extends AnyVal {
    // wild abuse of nomenclature nonwithstanding
    def flatLift(x: Int, y: Int): Option[Cell] = {
      if (x < 0 || x >= grid.length) None
      else {
        val inner = grid(x)
        if (y <= 0 || y >= inner.length) None
        else inner(y)
      }
    }


    def freeNeighbours(ii: Int)(jj: Int): List[(Int, Int)] = List(
        (ii - 1, jj + 1), (ii    , jj + 1), (ii + 1, jj + 1),
        (ii - 1, jj    ),                   (ii + 1, jj    ),
        (ii - 1, jj - 1), (ii    , jj - 1), (ii + 1, jj - 1),
      ).filter{ case(x, y) => (grid.isDefinedAt(x) && grid(x).isDefinedAt(y)) && !grid(x)(y).isDefined }


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


    def isSurrounded(ii: Int)(jj: Int): Boolean = {
      List(
        (ii - 1, jj + 1), (ii    , jj + 1), (ii + 1, jj + 1),
        (ii - 1, jj    ),                   (ii + 1, jj    ),
        (ii - 1, jj - 1), (ii    , jj - 1), (ii + 1, jj - 1),
      ).filter{ case(x, y) => (grid.isDefinedAt(x) && grid(x).isDefinedAt(y) && !grid(x)(y).isDefined) }.size > 0
    }
  }


  implicit class CellOps(val cell: Array[Double]) extends AnyVal {
    def control   = cell(Cell.CONTROL).toInt
    def state     = cell.slice(Cell.CONTROL + 1, cell.size)
    def energy    = cell(Cell.ENERGY)
    def cytoplasm = cell(Cell.CYTO)
    def SUCC      = cell(Cell.SUCC)
    def signals   = cell.slice(Cell.SIGNAL, cell.size)
    def clear: Unit = for(ii <- 0 until cell.size){cell(ii) = 0.0}
  }


  abstract class Mold {

    // this should be baked into diffusionRateLookup
    lazy val diffusionFactor = hardcode(0.2)
    lazy val osmosisTransfer = hardcode(0.1)

    lazy val splitThreshold = hardcode(10.0)
    lazy val killThreshold = hardcode(0.4)

    lazy val consumptionSpeed = hardcode(0.1)

    val diffusionRates: Array[Double] = List.fill(10)(diffusionFactor).toArray

    val liveCells : ArrayBuffer[(Int, Int)]
    val genomes   : ArrayBuffer[CellControl]
    var cells     : Array[Array[Option[Cell]]]
    var cellsB    : Array[Array[Option[Cell]]]
    val food      : Array[Array[Double]]


    // read: points
    var sporesReleased = 0.0


    /** Transfer based on osmosis */
    private def transfer(ii: Int, jj: Int, recipient: Cell): Unit = {

      // For efficiency we use a recipient cell rather than folding a bunch of cells
      def transferSingle(center: Cell, neighbor: Cell, recipient: Cell): Unit = {
        val cytoTransfer = ((neighbor.cytoplasm + center.SUCC) - (center.cytoplasm + neighbor.SUCC))*osmosisTransfer

        // Move cytoplasm
        recipient(Cell.CYTO) += cytoTransfer

        // Recalculate concentrations
        if(cytoTransfer > 0.0){
          val oldCyto = center.cytoplasm/(center.cytoplasm + neighbor.cytoplasm)
          val newCyto = 1.0 - oldCyto
          for(ii <- 3 until center.size){
            recipient(ii) = recipient(ii)*oldCyto + neighbor(ii)*newCyto
          }
        }
      }

      val cell = cells(ii)(jj).get
      cells.foreachNeighbours(ii)(jj)(transferSingle(cell,_,recipient)) 
    }



    /** Transfer based on diffusion. Does not transfer energy or cytoplasm, doesn't care about SUCC */
    private def diffuse(ii: Int, jj: Int, recipient: Cell): Unit = {

      def diffuseSingle(center: Cell, neighbor: Cell, recipient: Cell): Unit = {

        val centerCyto   = center.cytoplasm/(center.cytoplasm + neighbor.cytoplasm)
        val neighborCyto = 1.0 - centerCyto

        for(ii <- Cell.SIGNAL until center.size){
          // TODO: Diffusionrate and factor can (and will) be prebaked
          val loss = center(ii)*neighborCyto*diffusionRates(ii)
          val gain = neighbor(ii)*centerCyto*diffusionRates(ii)
          recipient(ii) = center(ii) + gain - loss
        }
      }

      val cell = cells(ii)(jj).get
      cells.foreachNeighbours(ii)(jj)(diffuseSingle(cell,_,recipient))
    }


    /**
      * Runs a single step of protein and cyto transfer
      * Diffusion moves protein gradients, whereas osmosis
      * moves cyto and energy (which in turn may carry proteins)
      */
    private def stepOsmosis: Unit = liveCells.foreach{ case(ii, jj) =>
      val recipient = cellsB(ii)(jj).get

      // Clear out contents of recieving buffer
      for(kk <- 0 until recipient.size){ recipient(kk) = cells(ii)(jj).get(kk) }
      transfer(ii, jj, recipient)
      diffuse(ii, jj, recipient)
    }



    private def stepConsumption: Unit = {
      liveCells.foreach{ case(x, y) =>
        if(food(x)(y) > 0.0){
          val energy = cellsB(x)(y).get(Cell.ENERGY)
          val cyto   = cellsB(x)(y).get(Cell.CYTO)
          val consumption = math.min(math.max(cyto, 10.0) * consumptionSpeed, food(x)(y))

          food(x)(y) -= consumption
          cellsB(x)(y).get(Cell.ENERGY) += consumption
        }
      }
    }



    private def populateLiveCells: Unit =
      for(ii <- 0 until cells.size){
        for(jj <- 0 until cells.size){
          if(cells(ii)(jj).isDefined) liveCells.append((ii, jj))
        }
      }


    /** Splits a cell in two if its pressure is over a threshold and it has free neighbors */
    private def addCell: Unit = {
      def eligible: Option[(Int, Int)] = liveCells
        .filter{ case(x,y) => cells.isSurrounded(x)(y) }
        .maxByOption{  case(x,y) => cellsB(x)(y).get(Cell.CYTO) }


      eligible.foreach{ case(x, y) if (cells(x)(y).get(Cell.CYTO) > splitThreshold) =>

        say("adding cell!")
        val free = cells.freeNeighbours(x)(y)
        val (sx, sy) = free(util.Random.nextInt(free.length))

        val selected = cellsB(x)(y).get
        val newCell  = cellsB(x)(y).get.clone

        selected(Cell.CYTO) *= 0.5
        selected(Cell.ENERGY) *= 0.5

        newCell(Cell.CYTO) *= 0.5
        newCell(Cell.ENERGY) *= 0.5

        newCell(Cell.CONTROL) = genomes.size

        val newCellA  = cellsB(x)(y).get.clone

        cells(sx)(sy) = Some(newCellA)
        cellsB(sx)(sy) = Some(newCell)

        val nextGenome = genomes(selected(Cell.CONTROL).toInt).nodes.clone
        val nextSMGC = CellControl(nextGenome)
        genomes.append(nextSMGC)
        liveCells.append((sx, sy))
        say("Cell was added!")

        case _ => ()
      }
    }


    /** 
      * Kills all cells whose cytoplasma is under a certain treshhold.
      * Contents are lost (subject to change) 
      */
    private def pruneCells: Unit = {
      for(ii <- (liveCells.length - 1) downto 0){
        val (x, y) = liveCells(ii)
        val cell = cells(x)(y).get
        if(cell(Cell.CYTO) < killThreshold){
          genomes(cell(Cell.CONTROL).toInt) = null
          liveCells.remove(ii)
          cells(x)(y) = None
          cellsB(x)(y) = None
        }
      }
    }


    /**
      * Alters cell state based on genome output.
      * (CellState, Genome) => (CellState, Genome)
      */
    private def runGenomes: Unit = {
      def runGenome(recipient: Cell): Unit = {
        val genome      = genomes(recipient.control)
        val (_, output) = genome.run(recipient.state)

        // def instead of val means this will change on each invocation
        // represents how much the cell can do at most
        def efficiency = recipient(Cell.ENERGY).max1


        /** Generate cytoplasm, consumes energy */
        recipient(Cell.CYTO) += efficiency*output(0).max1
        if(output(0) > 0.0)
          recipient(Cell.ENERGY) -= efficiency*output(0).max1
        else
          recipient(Cell.ENERGY) -= output(0).max1*0.25 // Quarter price refund


        /** Generate SUCC, consumes energy */
        if(output(1) > 0.0){
          recipient(Cell.SUCC)   += efficiency*output(1).max1
          recipient(Cell.ENERGY) -= efficiency*output(1).max1
        }
        else
          recipient(Cell.SUCC) += efficiency*output(1).max1

        /** Generate proteins and such */
        for(ii <- 2 until output.size){
          recipient(ii + Cell.SIGNAL) += output(ii).max1
        }
      }

      liveCells.foreach{ case(ii, jj) =>
        runGenome(cells(ii)(jj).get)
      }
    }

    def totalCyto  : Double = cells.flatten.flatten.map(cell => cell(Cell.CYTO)).sum
    def totalCytoB : Double = cellsB.flatten.flatten.map(cell => cell(Cell.CYTO)).sum


    def stepFull: Unit = {
      stepOsmosis
      runGenomes
      addCell
      pruneCells

      var tmp = cells
      cells   = cellsB
      cellsB  = tmp
    }

  }
  object Mold {

    def populateFood(dim: Int): Array[Array[Double]] = {

      val indices: List[(Int, Int)] = (0 until dim).flatMap{ x => (0 until dim).map((x, _)) }.toList
      val food = Array.ofDim[Double](dim, dim)

      def getRingNo(x: Int, y: Int) =
        math.max(math.abs(x - dim/2), math.abs(y - dim/2))

      for(ringNo <- 1 until (dim/2 - 1)){
        val eligible    = util.Random.shuffle(indices.filter{ case(x,y) => getRingNo(x,y) == ringNo })
        val foodSquares = (10 - ringNo).minClamp(2)
        val taken       = eligible.take(foodSquares)

        if(ringNo > dim/3)
          taken.foreach{ case(x, y) =>
            food(x)(y)     = ringNo
            food(x+1)(y)   = ringNo
            food(x-1)(y)   = ringNo
            food(x)(y+1) = ringNo
            food(x)(y-1) = ringNo
          }
        else
          taken.foreach{ case(x, y) => food(x)(y) = ringNo }
      }

      food
    }

    def populateFoodA(dim: Int): Array[Array[Double]] = {
      val food = Array.ofDim[Double](dim, dim)
      val maxDist = math.sqrt((2*dim*dim).toDouble)
      for(ii <- 0 until dim){
        for(kk <- 0 until dim){
          val distToCenter   = math.sqrt(math.pow(ii - dim/2, 2) + math.pow(kk - dim/2, 2))
          val hurr = 1.0 - math.pow(distToCenter/maxDist, 0.20)
          // val hurr = 1.0 - distToCenter/maxDist
          val shouldHaveFood = util.Random.nextDouble < hurr
          // val shouldHaveFood = true
          if(shouldHaveFood)
            food(ii)(kk) = distToCenter
        }
      }
      food
    }

    def apply(dim: Int): Mold = {
      val ded: Option[Cell] = None
      val m = new Mold {
        override var cells     = List.fill(dim)(List.fill(dim)(ded).toArray).toArray
        override var cellsB    = List.fill(dim)(List.fill(dim)(ded).toArray).toArray
        override val liveCells = new ArrayBuffer[(Int, Int)]()
        override val genomes   = new ArrayBuffer[CellControl]()
        override val food      = populateFood(dim)
      }

      val signals = Array.fill(5)(1.0)

      //                           CONTROL  SUCC,  cyto,  energy
      m.cells(15)(15) =  Some(Array(0.0,    1.0,   80.0,  1.0) ++ signals.clone)

      (0 until m.cells.size).foreach{ _ =>
        m.genomes.append(CellControl.random(30))
      }

      // These values does not matter
      m.cellsB(15)(15) =  Some(Array(0.0,    1.0,   80.0,  1.0) ++ signals.clone)
      m.populateLiveCells
      m
    }
  }
}
