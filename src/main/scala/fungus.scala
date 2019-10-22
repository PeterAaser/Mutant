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

  signals           : Vector[Double],
  nextSignals       : Array[Double]
) extends GridContent{

  def next: Cell = copy(
    cyto    = nextCyto,
    signals = nextSignals.clone.toVector
  )
}
case object Free extends GridContent
case object Wall extends GridContent

/** 
  * food is defined by its quantity and the signals (smells) it leaves
  */
case class Food(
  var amount : Double,
  signals    : Array[Double]
){
}

object Fungus {

  abstract class Mold(implicit params: Params) {
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
      * Runs a single step of protein and cyto transfer
      * Diffusion moves protein gradients, whereas osmosis
      * moves cyto (which in turn may carry signal proteins)
      */
    // private def stepOsmosis: Unit = liveCells.foreach{ case(ii, jj) =>
    //   transfer(ii, jj)
    //   diffuse(ii, jj)
    // }



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

      // def eligible2 = liveCells
      //   .filterNot{ case(ii,jj) => cells.isSurrounded(ii)(jj) }
      //   .filter{ case(ii,jj) => cells(ii)(jj) match { case Cell(state, _) => state(CellState.CYTO) > splitThreshold}}

      /**
        * Collect eligible cells, ordered from most 
        */
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

        // val nextCell = Cell(state.updated(CellState.CONTROL, genomes.size), nextState.clone)
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
      * Kills all cells whose cytoplasma is under a certain treshhold.
      * Contents are distributed to neighboring cells
      */
    private def pruneCells: Unit = {
      for(ii <- (liveCells.length - 1) downto 0){
        val (x, y) = liveCells(ii)
        cells(x)(y) match {
          case Cell(state, nextState) => {
            if(state(CellState.CYTO) < killThreshold){
              genomes(state(CellState.CONTROL).toInt) = null // lel

              if(state(CellState.CYTO) > 0){
                val neighbors = cells.neighborCells(x)(y)
                neighbors.foreach{ cell =>
                  cell.nextState(CellState.CYTO) += nextState(CellState.CYTO)/(neighbors.size.toDouble)
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
        val genome      = genomes(recipient.state.control)
        val (_, output) = genome.run(recipient.state.toArray)

        val outSUCC     = output(0)
        val outSPORE    = output(1)
        val signalStart = 2
        val genomeSize  = genome.nodes.size

        def efficiency = recipient.state(CellState.CYTO).max1

        /** Generate spores, consuming cyto */
        // if(outSPORE > 0.0){
        //   recipient.nextState(CellState.CYTO) -= (efficiency*outSPORE).max1
        //   this.spores += (efficiency*outSPORE).max1
        // }

        /**
          * Generate SUCC, consumes some cyto
          * Lowering SUCC is free.
          */
        if(outSUCC > 0.0){
          recipient.nextState(CellState.SUCC) += (efficiency*output(1)).roof(2.0)
          recipient.nextState(CellState.CYTO) -= (efficiency*output(1)).roof(2.0)*SUCCcost
        }
        else
          recipient.nextState(CellState.SUCC) += (efficiency*output(1)).roof(2.0)


        /** Generate proteins and such */
        for(ii <- signalStart until output.size){
          recipient.nextState(ii + CellState.SIGNAL) += output(ii).max1
        }
      }

      liveCells.foreach{ case(ii, jj) =>
        cells(ii)(jj) match { case x: Cell => runGenome(x) }
      }
    }

    /** Drains cyto to pay the rent */
    private def drainCyto: Unit = {
      liveCells.foreach{ case(x, y) =>
        cells(x)(y) match {
          case Cell(state, nextState) => {
            val cyto = state(CellState.CYTO)
            nextState(CellState.CYTO) -= cyto*rent
          }
        }
      }
    }

    // def totalCyto  : Double = cells.flatten.flatten.map(cell => cell(Cell.CYTO)).sum
    def totalCyto = cells.flatten.collect{ case Cell(state, nextState) => nextState(CellState.CYTO) }.sum

    def stepFull: Unit = {

      liveCells.map{ case(z, zz) => cells(z)(zz)}.collect{ case Cell(state, nextState) => state(CellState.CYTO) }.filter(_ < 0).foreach(say(_))


      dsay(s"at start:            $totalCyto")            
      stepOsmosis                             
      dsay(s"after osmosis        $totalCyto")        
      stepConsumption                         
      dsay(s"after consumption    $totalCyto")    
      drainCyto                               
      dsay(s"after drain          $totalCyto")          
      dsay(s"spores b4 genome     $spores")
      runGenomes                              
      dsay(s"after running genome $totalCyto") 
      dsay(s"spores afteer genome $spores")
      addCell                                 
      dsay(s"after adding cell    $totalCyto\n\n")    

      for(ii <- 0 until cells.size){
        for(jj <- 0 until cells.size){
          cells(ii)(jj) match {
            case x: Cell => cells(ii)(jj) = x.next
            case _ => ()
          }
        }
      }

      dsay(s"g $totalCyto")
      pruneCells
      dsay(s"h $totalCyto")

      for(ii <- 0 until cells.size){
        for(jj <- 0 until cells.size){
          cells(ii)(jj) match {
            case x: Cell => cells(ii)(jj) = x.next
            case _ => ()
          }
        }
      }
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

    def populateFood(dim: Int, abundance: Double = 1.0): Array[Array[Double]] = {

      val indices: List[(Int, Int)] = (0 until dim).flatMap{ x => (0 until dim).map((x, _)) }.toList
      val food = Array.ofDim[Double](dim, dim)

      def getRingNo(x: Int, y: Int) =
        math.max(math.abs(x - dim/2), math.abs(y - dim/2))

      food(dim/2)(dim/2) = 10.0*abundance

      for(ringNo <- 1 until (dim/2 - 1)){
        val eligible    = util.Random.shuffle(indices.filter{ case(x,y) => getRingNo(x,y) == ringNo })
        val foodSquares = (10 - ringNo).minClamp(2)
        val taken       = eligible.take(foodSquares)

        if(ringNo > dim/3)
          taken.foreach{ case(x, y) =>
            food(x)(y)   = ringNo*abundance
            food(x+1)(y) = ringNo*abundance
            food(x-1)(y) = ringNo*abundance
            food(x)(y+1) = ringNo*abundance
            food(x)(y-1) = ringNo*abundance
          }
          else
            taken.foreach{ case(x, y) => food(x)(y) = ringNo*abundance }
      }
      def sum = food.map(_.sum).sum
      // say(s"made a petri with $sum food on it")
      food
    }


    def apply(params: Params, genome: CellControl): Mold = {
      val cellz: Array[Array[GridContent]] = Array.ofDim[GridContent](params.dim, params.dim)
      for(ii <- 0 until params.dim)
        for(jj <- 0 until params.dim)
          cellz(ii)(jj) = Free

      val m = new Mold()(params) {
        override val cells     = cellz
        override val liveCells = new ArrayBuffer[(Int, Int)]()
        override val genomes   = new ArrayBuffer[CellControl]()
        override val food      = populateFood(params.dim, 3.0)
        // override val food      = spiralFood(params.dim, 10.0)
      }

      val signals = Array.fill(params.nSignals)(0.0)

      val initSUCC = 0.0
      val initCYTO = 8.0

      m.cells(params.dim/2)(params.dim/2) = Cell(Array(0.0, initCYTO, initSUCC) ++ signals.clone)
      m.liveCells.append((params.dim/2, params.dim/2))
      m.genomes.append(genome)
      m
    }
  }
}
