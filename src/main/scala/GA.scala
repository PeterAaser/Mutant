package scalapagos

import utils._
import IntBonusOps._
import DoubleBonusOps._
import cats._
import cats.implicits._
import collection.mutable.ArrayBuffer

case class GAParams(
  genomeInit : Int      = 1000,
  steps      : Int      = 5000,
  pop        : Int      = 20,
  keepPerGen : Int      = 2,

  mutationRate : Double = 0.10,

  iterations : Int = 1000
)

case class FungusParams(
  diffusionFactor  : Double = 0.05,
  osmosisTransfer  : Double = 0.05,
  splitThreshold   : Double = 1.0,
  killThreshold    : Double = 0.3,
  maxKillThreshold : Double = 1.0,
  consumptionSpeed : Double = 0.4,
  SUCCcost         : Double = 0.01,
  initCyto         : Double = 8.0,

  rent             : Double = 0.05,
  dim              : Int    = 120,

  nSignals         : Int    = 20,
  foodSignals      : Int    = 3,
)


object GA {

  implicit val hurr = GAParams()
  implicit val durr = FungusParams()
  import hurr._

  /**
    * Currently we're not memoizing
    */
  class Result(var score: Double, var memoized: Boolean, var genome: Array[Node]){

    def run: Unit = {
      val cc   = CellControl(genome.clone)
      val mold = Fungus.Mold(cc)
      val initFood = mold.food.map(_.map(_.amount).sum).sum
      for(ii <- 0 until steps){ mold.stepFull }
      val consumedFood = mold.food.map(_.map(_.amount).sum).sum
      score = initFood - consumedFood
    }
  }
  object Result{
    def apply(genome: Array[Node]): Result = new Result(0.0, false, genome)
  }


  val initSize = 400
  // val initSize = 100
  // val initSize = 10

  say("generating init genomes...")
  def initGenomes = (0 until initSize).map{ i =>

    val genome = CellControl.random(genomeInit).nodes
    val cc     = CellControl(genome.clone)
    val mold   = Fungus.Mold(cc)

    for(ii <- 0 until 100){ mold.stepFull }

    (genome, mold.spores)
  }.sortBy(_._2)
    .takeRight(20)
    .map{ case(genome, score) => new Result(score, true, genome) }
    .toArray


  def step(genomes: Array[Result]): Array[Result] = {
    import scala.collection.parallel.CollectionConverters._

    (0 until genomes.size).toList.par.map{ idx =>
      genomes(idx).run
    }

    val best = genomes.sortBy(_.score).takeRight(4)
    val next = best.flatMap{r => 
      val genome = r.genome
      val next = Array.fill(1)(Genetics.mutate(genome)).map(Result(_))
      Array(r) ++ next
    }
    say(s"scores: ${genomes.sortBy(_.score).map(_.score).toList}")

    next
  }


  def gogo: Unit = {
    val done = (0 to iterations).foldLeft(initGenomes){ case(g, iteration) =>
      // val cc = CellControl(g.reverse.head.genome)
      // val mold = Fungus.Mold(Params(), cc)
      // // mold.debugPrint = true
      // for(ii <- 0 until 500){
      //   if(mold.totalCyto > 0){
      //     say(PrintUtils.printPetri(mold.cells, mold.food))
      //     mold.stepFull
      //   }
      // }
      step(g)
    }

    val cc = CellControl(done.reverse.head.genome)
    val mold = Fungus.Mold(cc)
    mold.debugPrint = true
    for(ii <- 0 until 500){
      // say(PrintUtils.printPetri(mold.cells, mold.food))
      mold.stepFull
    }
  }
}
