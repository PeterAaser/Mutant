package scalapagos

import utils._
import IntBonusOps._
import DoubleBonusOps._
import cats._
import cats.implicits._
import collection.mutable.ArrayBuffer
import GridOps._

trait GridContent
case class Cell(
  genomeIndex       : Int,

  var cyto          : Double,
  var nextCyto      : Double,

  /** Range [0, 1] */
  var viscosity     : Double,
  var nextViscosity : Double,

  /** Range [0.5, 1.5] */
  var SUCC          : Double,
  var nextSUCC      : Double,

  /** 
    * A cells desire to split 
    * Range [0, 1]
    */
  var integrity     : Double,
  var nextIntegrity : Double,

  /**
    * How much cyto has flowed through the cell.
    * In order to be less time variable this should
    * probably be exp decay or something
    */
  var flow          : Double,

  signals           : Array[Double],
  nextSignals       : Array[Double]
)(implicit params: FungusParams) extends GridContent{

  def update: Unit = {
    for(ii <- 0 until signals.size){ signals(ii) = nextSignals(ii) }
    cyto      = nextCyto
    viscosity = nextViscosity
    SUCC      = nextSUCC
    integrity = nextIntegrity
  }
}
object Cell {
  def init(idx: Int)(implicit params: FungusParams): Cell = {
    Cell(idx, params.initCyto, params.initCyto, 0.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.0, Array.fill(params.nSignals)(0.0), Array.fill(params.nSignals)(0.0))
  }
}

case object Free extends GridContent
case object Wall extends GridContent

/** 
  * food is defined by its quantity and the signals (smells) it leaves.
  * To simplify things food signals are always from 0 to n (so for instance
  * if food has 4 signals total then the smells will be index 0 to 3.)
  * 
  * Smell is defined as (signal * amount)/(distance squared)
  */
case class Food(
  var amount : Double,
  smells     : List[Double]
){
}
object Food{
  def apply(amount: Double)(implicit params: FungusParams): Food = {
    import params._
    val huh = 0.0 :: List.fill(foodSignals - 1)(util.Random.nextDouble()).sorted
    val dur = (huh zip huh.tail).map{ case(a, b) => b - a }
    Food(amount, dur)
  }
}

object Fungus {

  abstract class Mold(implicit params: FungusParams) {
    import params._


    // Les muterables
    val liveCells : ArrayBuffer[(Int, Int)]
    val genomes   : ArrayBuffer[CellControl]
    val cells     : Array[Array[GridContent]]
    val food      : Array[Array[Food]]

    implicit var debugPrint = false

    // read: points
    var spores = 0.0


    /** 
      * Advection is the transport of a substance or quantity by bulk motion (wikipedia).
      * 
      * Cells with pressure differentials transmit cytoplasma "wholesale", that is with
      * with signals and so on.
      */
    private def advection(ii: Int, jj: Int): Unit = {

      def transferSingle(recipient: Cell, neighbor: Cell): Unit = {

        val pressureDifferential = (neighbor.cyto * recipient.SUCC) - (recipient.cyto * neighbor.SUCC)
        val viscosityCoeff       = 1.0 - (recipient.viscosity * neighbor.viscosity)
        val cytoTransfer         = viscosityCoeff * pressureDifferential


        /** 
          * Recalculate concentrations for recipient cell 
          * Only necessary for the net receiver, losing cyto does not impact concentrations
          * 
          */
        if(cytoTransfer > 0.0){
          val recipientCytoRatio = recipient.cyto/(recipient.cyto + neighbor.cyto)
          val neighborCytoRatio  = 1.0 - recipientCytoRatio

          for(ii <- 0 until recipient.signals.size){
            val concentrationChange   = recipient.signals(ii)*recipientCytoRatio + neighbor.signals(ii)*neighborCytoRatio
            recipient.nextSignals(ii) = concentrationChange
          }
        }

        recipient.nextCyto += cytoTransfer
      }

      cells(ii)(jj) match { case x: Cell =>
        cells.foreachNeighbours(ii)(jj)(transferSingle(x, _)) }
    }


    def updateCells: Unit = { liveCells.map(cells.apply(_)).collect{ case cell: Cell => cell.update }; () }


    /** 
      * Transfer based on diffusion. Does not transfer cytoplasm, doesn't care about SUCC 
      * 
      * In addition to transfering nutrients, food signals are also moved, allowing nearby
      * food to be detected.
      */
    private def diffuse(ii: Int, jj: Int): Unit = {

      def diffuseSingle(recipient: Cell, neighbor: Cell): Unit = {
        for(ii <- 0 until recipient.signals.size){
          val inbound  = neighbor.cyto  * neighbor.signals(ii)
          val outbound = recipient.cyto * recipient.signals(ii)

          recipient.nextSignals(ii) = (inbound - outbound)*diffusionFactor
        }
      }

      cells(ii)(jj) match { case x: Cell =>
        cells.foreachNeighbours(ii)(jj)(diffuseSingle(x, _)) }
    }



    /**
      * Consumes food at a given square, turning it into cytoplasma.
      * This is signalled to the cell by releasing a signalling protein.
      */
    private def stepConsumption: Unit = {
      liveCells.foreach{ case(ii, jj) => if(food(ii)(jj).amount > 0.0)
        cells(ii)(jj) match { case cell: Cell =>

          val foodAvailable  = food(ii)(jj).amount
        
          val maxConsumption = math.min(cell.cyto*consumptionSpeed, foodAvailable)
          val consumption    = math.min(maxConsumption, 1.0)
        
          food(ii)(jj).amount -= consumption
          cell.nextCyto += consumption
        }
      }
    }


    /** Splits a cell in two if its pressure is over a threshold and it has free neighbors */
    private def addCell: Unit = {

      def eligible = liveCells
        .filterNot{ case(ii,jj) => cells.isSurrounded(ii)(jj) }
        .map{ case(ii, jj) => (cells(ii)(jj), ii, jj) }
        .collect{ case(cell: Cell, ii, jj) if(cell.cyto * (1.0 - cell.integrity) > splitThreshold) =>
          (cell, ii, jj)
        }
        .sortBy{ case (cell, _, _) => -(1.0 - cell.integrity)*cell.cyto }


      /**
        * Attempt to split the cells with high cyto first.
        * If an eligible cell finds itself surrounded it is skipped
        */
      eligible.foreach{ case(cell, ii, jj) if (cells.freeNeighbours(ii)(jj).size > 0) =>

        val free = cells.freeNeighbours(ii)(jj)
        val (sx, sy) = free(util.Random.nextInt(free.length))

        val newCell = cell.copy(
          genomeIndex = genomes.size,
          nextSignals = cell.nextSignals.clone
        )

        newCell.nextCyto *= 0.3
        cell.nextCyto    *= 0.7

        cells(sx)(sy) = newCell
        liveCells.append((sx, sy))

        val newGenome = genomes(cell.genomeIndex).nodes.clone
        val newSMGC = CellControl(newGenome)
        genomes.append(newSMGC)
      }
    }


    /**
      * Removes cells based on cytoplasma and integrity (i.e a cell can protect itself from removal
      * by maintaining a high integrity)
      * 
      * Contents are distributed to neighboring cells
      */
    private def pruneCells: Unit = {
      for(ii <- (liveCells.length - 1) downto 0){
        val (x, y) = liveCells(ii)
        cells(x)(y) match {
          case cell: Cell => {

            val pruneThresholdReached = ((cell.cyto * cell.integrity) < killThreshold) && (cell.cyto < maxKillThreshold)
            if(pruneThresholdReached){

              genomes(cell.genomeIndex) = null // lel, should probably compact this every now and then

              if(cell.cyto > 0){
                val neighbors = cells.neighborCells(x)(y)
                neighbors.foreach{ neighborCell =>
                  neighborCell.nextCyto += cell.cyto/(neighbors.size.toDouble)
                }
              }

              liveCells.remove(ii)
              cells(x)(y) = Free
            }
          }
        }
      }
    }


    /**
      * Alters cell state based on genome output.
      * (CellState, Genome) => (CellState, Genome)
      */
    private def runGenomes: Unit = {
      def runGenome(recipient: Cell): Unit = {

        val genome = genomes(recipient.genomeIndex)

        val inputs = Array(recipient.cyto, recipient.viscosity, recipient.SUCC, recipient.integrity, recipient.flow) ++ recipient.signals

        val (_, output) = genome.run(inputs)

        val outViscosity = output(0)
        val outSUCC      = output(1)
        val outIntegrity = output(2)

        val signalStart = 3
        val outSignals  = output.drop(signalStart)

        def efficiency = math.max(recipient.cyto, 1.0)

        /**
          * Generate SUCC, consumes some cyto
          * Lowering SUCC is free.
          * 
          * TODO: Conform check is rickety AF
          */
        if(outSUCC > 0.0){
          recipient.nextSUCC += (math.min(efficiency*outSUCC, 1.0))
          recipient.nextCyto -= (math.min(efficiency*outSUCC, 1.0))*SUCCcost
        }
        else{
          recipient.nextSUCC = math.min(recipient.nextSUCC + math.max(efficiency*outSUCC, -1.0), 0.0)
        }

        if(recipient.nextSUCC > 1.5)
          recipient.nextSUCC = 1.5
        else if(recipient.nextSUCC < 0.5)
          recipient.nextSUCC = 0.5


        /** Generate proteins and such */
        for(ii <- 0 until outSignals.size){
          val outputClamped     = if(output(ii) > 1.0) 1.0 else if(output(ii) < -1.0) -1.0 else output(ii)
          val nextConcentration = recipient.nextSignals(ii) + output(ii)

          if(nextConcentration > 0.0)
            recipient.nextSignals(ii) = nextConcentration
          else
            recipient.nextSignals(ii) = 0.0
        }

        // TODO: Make this less rickety
        val nextViscosity = recipient.viscosity + outViscosity
        recipient.nextViscosity = if(nextViscosity < 0.0) 0.0 else if(nextViscosity > 1.0) 1.0 else nextViscosity
      }

      liveCells.foreach{ case(ii, jj) =>
        cells(ii)(jj) match { case x: Cell => runGenome(x) }
      }
    }


    /** Drains cyto to pay the rent */
    private def drainCyto: Unit = {
      val rentInv = 1.0 - rent
      liveCells.foreach{ case(x, y) =>
        cells(x)(y) match { case cell: Cell => { cell.nextCyto *= rentInv } }
      }
    }

    def totalCyto = cells.flatten.collect{ case cell: Cell => cell.nextCyto }.sum

    def stepFull: Unit = {

      dsay(s"at start: $totalCyto")            
      liveCells.foreach{ case(ii, jj) =>
        advection(ii, jj)
        diffuse(ii, jj)
      }

      stepConsumption
      dsay(s"after consumption    $totalCyto")    

      drainCyto                               
      dsay(s"after drain          $totalCyto")          

      runGenomes                              
      dsay(s"after running genome $totalCyto") 

      updateCells

      addCell                                 
      dsay(s"after adding cell    $totalCyto")    

      updateCells

      dsay(s"g $totalCyto")
      pruneCells
      dsay(s"h $totalCyto")

      updateCells
    }
  }
  object Mold {

    def spiralFood(dim: Int, abundance: Double = 1.0): Array[Array[Double]] = {

      val food = Array.ofDim[Double](dim, dim)

      food(dim/2)(dim/2) = 3.0*abundance

      def addStrip(start: (Int, Int), direction: (Int, Int), length: Int): (Int, Int) = {
        var (x, y) = start
        val (dx, dy) = ((direction._1 * length) + x, (direction._2 * length) + y)
        while( (x != dx) || (y != dy) ){
          if(food.isDefinedAt(x) && food(x).isDefinedAt(y)){
            food(x)(y) = abundance
          }
          x += direction._1
          y += direction._2
        }
        (x, y)
      }

      def addStrips(start: (Int, Int), direction: (Int, Int), length: Int): Unit = {
        val (x, y) = start
        if(food.isDefinedAt(x) && food(x).isDefinedAt(y)){
          val (nextx, nexty) = addStrip(start, direction, length)
          val (nextdx, nextdy) = direction match {
            case ( 1,  0) => ( 0, -1)
            case ( 0, -1) => (-1,  0)
            case (-1,  0) => ( 0,  1)
            case ( 0,  1) => ( 1,  0)
          }
          if(!((nextx == x) && (nexty == y)))
            addStrips((nextx, nexty), (nextdx, nextdy), length + 2)
        }
      }

      addStrips((dim/2, dim/2), (1, 0), 3)
      food
    }


    def populateFood(abundance: Double = 1.0)(implicit params: FungusParams): Array[Array[Food]] = {

      import params._

      val indices: List[(Int, Int)] = (0 until dim).flatMap{ x => (0 until dim).map((x, _)) }.toList
      val food = Array.ofDim[Food](dim, dim)

      def getRingNo(x: Int, y: Int) =
        math.max(math.abs(x - dim/2), math.abs(y - dim/2))

      food(dim/2)(dim/2) = Food(10.0*abundance)

      for(ringNo <- 1 until (dim/2 - 1)){
        val eligible    = util.Random.shuffle(indices.filter{ case(x,y) => getRingNo(x,y) == ringNo })
        val foodSquares = (10 - ringNo).minClamp(2)
        val taken       = eligible.take(foodSquares)

        if(ringNo > dim/3)
          taken.foreach{ case(x, y) =>
            food(x)(y)   = Food(ringNo*abundance)
            food(x+1)(y) = Food(ringNo*abundance)
            food(x-1)(y) = Food(ringNo*abundance)
            food(x)(y+1) = Food(ringNo*abundance)
            food(x)(y-1) = Food(ringNo*abundance)
          }
          else
            taken.foreach{ case(x, y) => food(x)(y) = Food(ringNo*abundance) }
      }
      def sum = food.map(_.map(_.amount).sum).sum
      // say(s"made a petri with $sum food on it")
      food
    }


    def apply(genome: CellControl)(implicit params: FungusParams): Mold = {
      val cellz: Array[Array[GridContent]] = Array.ofDim[GridContent](params.dim, params.dim)
      for(ii <- 0 until params.dim)
        for(jj <- 0 until params.dim)
          cellz(ii)(jj) = Free

      
      cellz(params.dim/2)(params.dim/2) = Cell.init(0)

      val m = new Mold()(params) {
        override val cells     = cellz
        override val liveCells = new ArrayBuffer[(Int, Int)]()
        override val genomes   = new ArrayBuffer[CellControl]()
        override val food      = populateFood(3.0)
      }

      m.liveCells.append((params.dim/2, params.dim/2))
      m.genomes.append(genome)

      val signals = Array.fill(params.nSignals)(0.0)

      m
    }
  }
}
