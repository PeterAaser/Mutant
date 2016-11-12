package GA
import scala._
import scalaz._
import Scalaz._

import scala.util.Random
import scala.language.postfixOps

import Data._

import Representations._


object Representations {

    // Collect a unique sample of some range
    def randomSample(start: Int, stop: Int, points: Int): List[Int] = {
        def roll(start: Int, stop: Int, points: Int, collected: Set[Int]): Set[Int] = {
            val needed = points - collected.size
            if (needed > 0){
                val samples = (collected /: (List.fill(needed)(Random.nextInt( (stop - start) + start ))) )(_+_)
                roll(start, stop, points, samples) 
            }
            else
                collected
        }
        roll(start, stop, points, Set[Int]()).toList
    }


    case class BitGene(bits: Vector[Boolean], crossRate: Double, mutationSeverity: Double){
        override def toString: String = bits.map(if(_) 1 else 0).mkString("[", ",", "]")

        // def reify: Int = (0 /: bits.map(if(_) 1 else 0))( (bits, b) => (b | (bits << 1)) )
        lazy val toInt = java.lang.Integer.parseInt(bits.map(if(_) 1 else 0).mkString, 2)
        lazy val maxInt = (1 << bits.length) - 1
        lazy val toDouble = toInt.toDouble/maxInt
    }
    object BitGene {

        def init(n: Int, crossRate: Double, mutationSeverity: Double): BitGene = 
            BitGene(Vector.fill(n)(Random.nextBoolean), crossRate, mutationSeverity)

        implicit val bitGene = new Gene[BitGene] {
            
            def cross(g1: BitGene, g2: BitGene): (BitGene, BitGene) = {
                val (t1, t2) = (g1.bits.toArray, g2.bits.toArray)

                for(i <- 0 until (g1.bits.length*g1.crossRate).toInt){
                    val crossPoint = Random.nextInt(g1.bits.length - 1)
                    val temp = t1(crossPoint)
                    t1(crossPoint) = t2(crossPoint)
                    t2(crossPoint) = temp
                }
                (g1.copy(bits=t1.toVector), g2.copy(bits=t2.toVector))
            }

            def mutate(g1: BitGene): BitGene = {
                val t1 = g1.bits.toArray

                for(i <- 0 until (g1.bits.length*g1.mutationSeverity).toInt){
                    val mPoint = Random.nextInt(g1.bits.length) 
                    t1(mPoint) = !t1(mPoint)
                }
                g1.copy(bits=t1.toVector)
            }
        }
    }


    case class SingleBitGenome(gene: BitGene, mutationRate: Double)
    object SingleBitGenome {

        def initPool(poolSize: Int, genomeSize: Int, crossRate: Double, mutationRate: Double, mutationSeverity: Double): IndexedSeq[SingleBitGenome] = 
            Vector.fill(poolSize)(SingleBitGenome(BitGene.init(genomeSize, crossRate, mutationSeverity), mutationRate))

        implicit val singleBitGenome = new Genome[SingleBitGenome] {

            def cross(genome1: SingleBitGenome, genome2: SingleBitGenome): (SingleBitGenome, SingleBitGenome) = {
                val (gene1, gene2) = genome1.gene.cross(genome2.gene)
                (genome1.copy(gene = gene1), genome2.copy(gene = gene2))
            }

            def mutate(genome1: SingleBitGenome): SingleBitGenome = {
                if (Random.nextDouble < genome1.mutationRate){
                    genome1.copy(gene = genome1.gene.mutate)
                }
                else genome1
            }
        }
    }


    case class MultiBitGenome(
        genes: Seq[BitGene], 
        crossover: (MultiBitGenome, MultiBitGenome) => (MultiBitGenome, MultiBitGenome),
        mutate: MultiBitGenome => MultiBitGenome
    ){
        override def toString: String = genes.mkString("[", " ", "]")
        
    }
    object MultiBitGenome {

        def init(
            amountGenes: Int, 
            geneLength: Int,
            crossover: (MultiBitGenome, MultiBitGenome) => (MultiBitGenome, MultiBitGenome),
            mutate: MultiBitGenome => MultiBitGenome
        ): MultiBitGenome = 
            MultiBitGenome(Vector.fill(amountGenes)(BitGene.init(geneLength, .0, .0)), crossover, mutate)

        implicit val multiBitGenome = new Genome[MultiBitGenome] {

            def cross(g1: MultiBitGenome, g2: MultiBitGenome): (MultiBitGenome, MultiBitGenome) =
                g1.crossover(g1, g2)

            def mutate(g1: MultiBitGenome): MultiBitGenome = 
                g1.mutate(g1)
        }

        def atomicGeneCrossover(crossRate: Double, g1: MultiBitGenome, g2: MultiBitGenome): (MultiBitGenome, MultiBitGenome) = {
            val (t1, t2) = (g1.genes.toArray, g2.genes.toArray)
            val crossovers = (g1.genes.size*crossRate).toInt
            // println(crossovers)

            for (i ←  0 until crossovers){
                val crossPoint = Random.nextInt(g1.genes.length - 1)
                val temp = t1(crossPoint)
                t1(crossPoint) = t2(crossPoint)
                t2(crossPoint) = temp
            }
            (g1.copy(genes = t1.toVector), g2.copy(genes = t2.toVector))
        }

        def mixGeneCrossover(crossRate: Double, g1: MultiBitGenome, g2: MultiBitGenome): (MultiBitGenome, MultiBitGenome) = {
            val (t1, t2) = (g1.genes.toArray, g2.genes.toArray)
            val crossovers = (g1.genes.size*crossRate).toInt

            for (i ←  0 until crossovers){
                val crossPoint = Random.nextInt(g1.genes.length - 1)

                val (λ1, λ2) = (t1(crossPoint).bits.toArray, t2(crossPoint).bits.toArray)

                for(i <- 0 until 3){
                    val crossPoint = Random.nextInt(λ1.length - 1)
                    val µ = λ1(crossPoint)
                    λ1(crossPoint) = λ2(crossPoint)
                    λ2(crossPoint) = µ
                }

                t1(crossPoint) = BitGene(λ1.toVector, .0, .0)
                t2(crossPoint) = BitGene(λ2.toVector, .0, .0)
            }
            (g1.copy(genes = t1.toVector), g2.copy(genes = t2.toVector))

        }

        def atomicGeneMutate(mutationSeverity: Double, mutationRate: Double, g1: MultiBitGenome): MultiBitGenome = {
            if(Random.nextDouble < mutationRate){
                val points = randomSample(0, g1.genes.length, (mutationRate*g1.genes.length).toInt)

                val mutant = (g1.genes /: points)( (λ, index) => {
                    λ.updated(index, BitGene.init(8, .0, .0))
                })
                g1.copy(genes = mutant)
            }
            else g1
        }
    }


    case class SymbolGene(symbol: Byte, s: Byte)
    object SymbolGene {

        def init(s: Byte): SymbolGene =
            SymbolGene(Random.nextInt(s).toByte, s)

        implicit val symbolGene = new Gene[SymbolGene] {

            def cross(g1: SymbolGene, g2: SymbolGene): (SymbolGene, SymbolGene) =
                (g2, g1)
            
            def mutate(g1: SymbolGene): SymbolGene = g1.copy(symbol = Random.nextInt(g1.s).toByte)
        }
    }


    case class SymbolGenome(
        genes: IndexedSeq[SymbolGene], 
        crossRate: Double, 
        mutationRate: Double,  
        mutationSeverity: Double
    ){
        override def toString: String = genes.map(_.symbol).mkString("[", ", ", "]")
    }
    object SymbolGenome {

        implicit val symbolGenome = new Genome[SymbolGenome] {

            def cross(genome1: SymbolGenome, genome2: SymbolGenome): (SymbolGenome, SymbolGenome) = {
                val (t1, t2) = (genome1.genes.toArray, genome2.genes.toArray)
                for(i <- 0 until(genome1.genes.length * genome1.crossRate).toInt){
                    val crossPoint = Random.nextInt(genome1.genes.length - 1)
                    val (g1, g2) = t1(crossPoint).cross(t2(crossPoint))
                    t1(crossPoint) = g2
                    t2(crossPoint) = g1
                }
                (genome1.copy(genes=t1.toVector), genome2.copy(genes=t2.toVector))
            }

            def mutate(genome1: SymbolGenome): SymbolGenome = {
                val t1 = genome1.genes.toArray

                for(i <- 0 until (genome1.genes.length * genome1.mutationRate).toInt){
                    val mPoint = Random.nextInt(genome1.genes.length)
                    t1(mPoint) = t1(mPoint).mutate
                }
                genome1.copy(genes=t1.toVector)
            }
        }

        def initGenome(
            genomeSize: Int, 
            symbols: Byte, 
            crossRate: Double, 
            mutationRate: Double,
            mutationSeverity: Double
        ): SymbolGenome = {

            SymbolGenome(
                Vector.fill(genomeSize)(SymbolGene.init(symbols)), 
                crossRate, 
                mutationRate, 
                mutationSeverity
            )
        }

        def initPool(
            poolSize: Int, 
            genomeSize: Int, 
            symbols: Byte, 
            crossRate: Double, 
            mutationRate: Double,
            mutationSeverity: Double
        ): IndexedSeq[SymbolGenome] = {

            Vector.fill(poolSize)(
                SymbolGenome.initGenome(
                    genomeSize, 
                    symbols, 
                    crossRate, 
                    mutationRate, 
                    mutationSeverity
                )
            )
        }
    }
}
