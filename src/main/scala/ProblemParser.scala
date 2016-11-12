package GA

import scala._
import scalaz._
import Scalaz._

import scala.io.Source
import spray.json._
import DefaultJsonProtocol._

import Representations._
import Scaling._
import Reproduction._
import ParentSelection._
import AdultSelection._

import Data._


object JParse {

    object MyJsonProtocol extends DefaultJsonProtocol {
        implicit val commonParamFormat = jsonFormat12(CommonParams)
        implicit val trackerParamFormat = jsonFormat4(TrackerParams)
    }

    import MyJsonProtocol._

    case class CommonParams(
        problem: String, 
        adultPool: Int, 
        adultLimit: Option[Int],
        childPool: Option[Int],
        crossRate: Double, 
        mutationRate: Double,
        mutationSeverity: Double,
        contestants: Option[Int],
        epsilon: Option[Double],
        generations: Int,
        reproductionStrat: String,
        populationStrat: String
    )

    case class TrackerParams(
        layout: List[Int],
        pull: Boolean,
        moves: Option[Int],
        toroidal: Boolean
    )

    def parseCTRNN(path: String): Runner[MultiBitGenome] = {

        val problemString = Source.fromFile("presets/" + path + ".json").getLines.mkString
        val p = problemString.parseJson.convertTo[CommonParams]
        val t = problemString.parseJson.convertTo[TrackerParams]

        type Pheno = Phenotype[MultiBitGenome]
        type Phenos = IndexedSeq[Pheno]

        val weightRange = ( -  5.0, 5.0) 
        val biasRange   = ( - 10.0, 0.0) 
        val gainRange   = (    1.0, 5.0) 
        val tauRange    = (    1.0, 2.0) 

        def layout = t.layout
        def generator(g: MultiBitGenome): ANN.CTRNN = ANN.CTRNN.fromGenome(
            weightRange,
            biasRange,
            gainRange,
            tauRange,

            layout,
            g
        )

        val updateAgent = if (t.toroidal) tracker.updateAgentToroidal _ else tracker.updateAgentToroidal _

        def evaluate(genome: MultiBitGenome): Double =
            tracker.evaluate(generator, genome, updateAgent)

        def grow(genome: MultiBitGenome): Pheno =
            Phenotype[MultiBitGenome](genome, evaluate(genome), evaluate(genome), 0)

        def reproduce(adults: Phenos): Vector[MultiBitGenome] = 
            sexualReproduction[MultiBitGenome](adults).toVector

        val reproductionStrat: (Int, Int, Phenos) => Phenos = (p.reproductionStrat, p.epsilon, p.contestants) match {
            case ( "tournament", Some(epsilon), Some(contestants) ) => tournamentStrat(_, _, _, epsilon, contestants)
            case ( "roulette", _, _) => rouletteStrat
            case ( "sigma", _, _) => sigmaSelect
            case ( "rank", _, _) => rankStrat
            case _ => rouletteStrat
        }

        def populationStrat = (p.populationStrat, p.childPool, p.adultLimit) match {
            
            case ("mixin", Some(childPool), _) => mixin[MultiBitGenome](
                p.adultPool,
                childPool,
                reproductionStrat,
                reproduce,
                sigmaSelect,
                genomes => genomes.par.map(grow(_)).toVector)

            case ("full", _, _) => full[MultiBitGenome](
                p.adultPool,
                reproductionStrat,
                reproduce,
                genomes => genomes.par.map(grow(_)).toVector)

            case ("mixinLimited", Some(childPool), Some(limit)) => mixinLimited[MultiBitGenome](
                p.adultPool,
                childPool,
                limit,
                reproductionStrat,
                reproduce,
                sigmaSelect,
                genomes => genomes.par.map(grow(_)).toVector)

            case ("over", _, _) => ???
        }

        def crossover(g1: MultiBitGenome, g2: MultiBitGenome): (MultiBitGenome, MultiBitGenome) = MultiBitGenome.mixGeneCrossover(p.crossRate, g1, g2)
        def mutate(g1: MultiBitGenome): MultiBitGenome = MultiBitGenome.atomicGeneMutate(p.mutationSeverity, p.mutationRate, g1)

        val genes = {
            (3 * (0 /: layout.tail)(_+_)) + // tau, theta, gainz
            layout.head * layout.tail.head + // input weights
            (0 /: (layout.tail zip (layout.tail.tail ::: List(0))))( (acc: Int , λ: (Int, Int)) => acc + (λ._1*λ._1) + (λ._1*λ._2)) // weights
        }

        println((genes*p.crossRate).toInt)

        val genomes = Vector.fill(p.adultPool)(MultiBitGenome.init(genes, 8, crossover, mutate)).map(grow(_))

        Runner[MultiBitGenome](
            genomes,
            pop => ( (pop.generation > (p.generations - 1) ) ),
            populationStrat
        )
    }
}
