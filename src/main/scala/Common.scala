package GA

import scala._
import scalaz._
import Scalaz._

import scala.util.Random
import scala.language.postfixOps

import Data._

import Representations._
import Scaling._
import Reproduction._
import ParentSelection._
import AdultSelection._

object Scaling {
 
    type Phenos[A] = IndexedSeq[Phenotype[A]]

    // Scales a population of candidates using some function that can be tailored to populations
    def scale[A: Genome](candidates: Phenos[A], scaler: Phenos[A] => (Double => Double)): Phenos[A] = {

            val scalingFun = scaler(candidates)
            candidates.map(c => c.copy(relativeFitness=scalingFun(c.relativeFitness)))  
        }


    def boltzmannScaler[A](candidates: Phenos[A], temperature: Int): (Double => Double) = {
        
        val fitnessTotal = math.exp(
            (0.0 /: candidates.map(_.relativeFitness))(_+_)/temperature.toDouble
        )
        (relativeFitness => math.exp(relativeFitness/temperature.toDouble))
        
    }



    def sigma[A](candidates: Phenos[A]): (Double => Double) = {
        val mean = (0.0 /: candidates.map(_.relativeFitness))(_+_)/(candidates.length.toDouble)
        if(mean == 0.0)
            (relativeFitness => relativeFitness)
        else{
            val stddev = (0.0 /: candidates.map(p => math.pow(p.relativeFitness - mean, 2)))(_+_)/(candidates.length.toDouble)
            (relativeFitness => relativeFitness*(1.0 + ((relativeFitness - mean) / (2.0 * stddev))))
        }
    }


    def normalizer[A](candidates: Phenos[A]): (Double => Double) = {
        val fittest = candidates
            .reduceLeft( (l, r) => if (l.relativeFitness > r.relativeFitness) l else r)

        (relativeFitness => relativeFitness/(fittest.relativeFitness))
    }


    def badnessNormalizer[A](candidates: Phenos[A]): (Double => Double) = {
        val unfittest = candidates
            .reduceLeft( (l, r) => if (l.relativeFitness > r.relativeFitness) l else r)

        (relativeFitness => 1.0 - relativeFitness/(unfittest.relativeFitness))
    }


    def rankScale[A: Genome](generation: Int, candidates: Phenos[A]): (Phenos[A]) = {
        val sorted = candidates.sortBy(_.relativeFitness).zipWithIndex
        val min = sorted.head._1.relativeFitness 
        val max = sorted.last._1.relativeFitness 

        def rscaler(rank: Int, fitness: Double): Double = {
            (min + (max - min)*(rank - 1)/(sorted.length - 1))
        }
        sorted.map(t => t._1.copy(relativeFitness = rscaler(t._2, t._1.relativeFitness)))
        
    }


    // creates a roulette scaled, normalized list of candidates
    def rouletteScaler[A: Genome](candidates: Phenos[A]): Phenos[A] = {

        val fitnessSum = (0.0 /: candidates.map(_.relativeFitness))(_+_)

        def stackingSum(ps: Phenos[A], stack: Double): Phenos[A] = ps match {
            case h +: t => h.copy( relativeFitness = (h.relativeFitness + stack)/fitnessSum) +: stackingSum(t, stack + h.relativeFitness)
            case _ => Vector[Phenotype[A]]()
        }
        stackingSum(candidates, 0.0)
        
    }


}

object Selection {

    type Phenos[A] = IndexedSeq[Phenotype[A]]

    def proportional[A](candidates: Phenos[A], spots: Int): Phenos[A] = 
            candidates.sortBy(_.relativeFitness) takeRight spots

    def proportionalMixin[A](gen: Int, spots: Int, children: Phenos[A], adults: Phenos[A]): Phenos[A] =
            (adults ++ children).sortBy(_.relativeFitness) takeRight spots

}

object ParentSelection {

    type Phenos[A] = IndexedSeq[Phenotype[A]]

    // Roulette selection expects a roulette scaled population
    def rouletteSelection[A](candidates: Phenos[A]): Int => Phenos[A] = 
        winners => {

            def search(low: Int, high: Int, target: Double): Phenotype[A] = {
                if (low == high - 1){
                    candidates(high)
                }
                else (low + high)/2 match {
                    case mid if candidates(mid).relativeFitness > target => search(low, mid, target)
                    case mid if candidates(mid).relativeFitness < target => search(mid, high, target)
                    case _ => candidates(low)
                }
            }

            Vector.fill(winners)(search(0, candidates.size, Random.nextDouble))
    }


    def tournamentSelection[A](candidates: Phenos[A], winners: Int, epsilon: Double, contestants: Int): Phenos[A] = {

        def select: Phenotype[A] = {
            if(Random.nextDouble < epsilon){
                candidates(Random.nextInt(candidates.size - 1))
            }
            else{
                tournament(candidates, contestants)
            }
        }

        def tournament(candidates: Phenos[A], contestants: Int): Phenotype[A] = {

            val chosen = sample(0 to candidates.size - 1 toList, contestants).map(candidates(_))
            chosen.reduceLeft( (l, r) => if (l.relativeFitness > r.relativeFitness) l else r)
        }

        Vector.fill(winners)(select)
    }

    // https://stackoverflow.com/questions/14862602/scala-java-generating-a-set-of-non-repeating-random-numbers
    def sample[A](items: List[A], sampleSize: Int) = {
        def collect(vect: Vector[A], sampleSize: Int, acc: List[A]): List[A] = {
            if(sampleSize == 0) acc
            else {
                val index = Random.nextInt(vect.size)
                collect( vect.updated(index, vect(0)) tail, sampleSize - 1, vect(index) :: acc)
            }
        }
        collect(items toVector, sampleSize, Nil)
    }

    def sigmaSelect[A: Genome](gen: Int, winners: Int, adults: Phenos[A]): Phenos[A] = {
            val sigmaScaled = scale[A](adults, sigma).sortBy(_.trueFitness)
            ParentSelection.rouletteSelection(rouletteScaler(sigmaScaled))(winners)
        }


    def tournamentStrat[A](gen: Int, winners: Int, adults: Phenos[A], epsilon: Double, contestants: Int): Phenos[A] =
        tournamentSelection(adults, winners, epsilon, contestants)


    def rouletteStrat[A: Genome](gen: Int, winners: Int, adults: Phenos[A]): Phenos[A] = 
        ParentSelection.rouletteSelection(rouletteScaler(adults))(winners)
        

    def rankStrat[A: Genome](gen: Int, winners: Int, adults: Phenos[A]): Phenos[A] = {
        val scaled = rankScale(gen, adults)
        rouletteSelection(rouletteScaler(scaled))(winners)
    }


    def boltzmannStrat[A: Genome](gen: Int, winners: Int, adults: Phenos[A]): Phenos[A] = {
        val scaler = (pop: Phenos[A]) => boltzmannScaler(pop, 160 - gen)
        val scaled = scale[A](adults, scaler)
        rouletteSelection(rouletteScaler(scaled))(winners)
    }

    def mysteryStrat[A: Genome](gen: Int, winners: Int, adults: Phenos[A], epsilon: Double, contestants: Int): Phenos[A] = {
        gen match {
            case x if x < 40 => rankStrat(gen, winners, adults)
            case x if x < 80 => rouletteStrat(gen, winners, adults)
            case x if x < 110 => tournamentSelection(adults, winners, epsilon, contestants)
            case _ => boltzmannStrat(gen, winners, adults)
        }
    }
}


object Reproduction {

    type Phenos[A] = IndexedSeq[Phenotype[A]]
   
    def asexual[A: Genome](p: Phenotype[A]): A =
        p.genome.mutate
    


    def sexual[A : Genome](p1: Phenotype[A], p2: Phenotype[A]): (A, A) = {

            val children = p1.genome.cross(p2.genome)
            (children._1.mutate, children._2.mutate)
    }


    def sexualReproduction[A: Genome](parents: Phenos[A]): IndexedSeq[A] = {

            def reproduce(parents: IndexedSeq[Phenotype[A]]): IndexedSeq[A] = {
                parents match {
                    case p1 +: p2 +: t => 
                        val children = sexual(p1, p2)
                        children._1 +: children._2 +: reproduce(t)
                    case _ => Vector[A]()
                }
            }
            reproduce(parents)
    }


    def asexualReproduction[A: Genome](parents: Phenos[A]): IndexedSeq[A] =
        parents.map(asexual(_))
    
}


object AdultSelection {

    type Phenos[A] = IndexedSeq[Phenotype[A]]
    type Genos[A] = IndexedSeq[A]
    
    def full[A: Genome](
        µ: Int,
        parentSel: (Int, Int, Phenos[A]) => Phenos[A],
        reproductionScheme: Phenos[A] => Genos[A],
        grow: Genos[A] => Phenos[A]
    ): Population[A] => Population[A] =
        pop => {
            val parents = parentSel( pop.generation, µ, pop.adults)
            val children = grow(reproductionScheme(parents))
            pop.copy(adults = children)
        }
    

    def overProduction[A : Genome](
        µ: Int,
        λ: Int,
        parentSel: (Int, Int, Phenos[A]) => Phenos[A],
        reproductionScheme: Phenos[A] => Genos[A],
        adultSel: (Int, Int, Phenos[A]) => Phenos[A],
        grow: Genos[A] => Phenos[A]
    ): Population[A] => Population[A] =
        pop => {
            val parents = parentSel( pop.generation, λ, pop.adults)
            val children = grow(reproductionScheme(parents))
            val survivors = adultSel( pop.generation, µ, children)
            pop.copy(adults = survivors)
        }


    def mixin[A: Genome](
        µ: Int,
        λ: Int,
        parentSel: (Int, Int, Phenos[A]) => Phenos[A],
        reproductionScheme: Phenos[A] => Genos[A],
        adultSel: (Int, Int, Phenos[A]) => Phenos[A],
        grow: Genos[A] => Phenos[A]
    ): Population[A] => Population[A] =
        pop => {
            val parents = parentSel( pop.generation, λ, pop.adults)
            val children = grow(reproductionScheme(parents))
            val survivors = adultSel( pop.generation, µ, (children ++ pop.adults))
            pop.copy(adults = survivors)
        }

    def mixinLimited[A: Genome](
        µ: Int,
        λ: Int,
        α: Int,
        parentSel: (Int, Int, Phenos[A]) => Phenos[A],
        reproductionScheme: Phenos[A] => Genos[A],
        adultSel: (Int, Int, Phenos[A]) => Phenos[A],
        grow: Genos[A] => Phenos[A]
    ): Population[A] => Population[A] =
        pop => {
            val parents = parentSel( pop.generation, λ, pop.adults)
            val children = grow(reproductionScheme(parents))
            val chosenParents = pop.adults.sortBy(_.trueFitness).takeRight(α)
            val survivors = adultSel( pop.generation, (µ - α), (children ++ pop.adults)) ++ chosenParents
            pop.copy(adults = survivors)
        }
}
