package GA

import scala._
import scalaz._
import Scalaz._

import Data._
import scala.language.existentials
import scala.language.higherKinds

object Data {

    trait Gene[A] {
        def cross(g1: A, g2: A): (A, A)
        def mutate(g1: A): A
    }
    implicit class GeneOps[A](a: A)(implicit ev: Gene[A]) {
        def cross(g2: A): (A, A) = ev.cross(a, g2)
        def mutate: A = ev.mutate(a)
    }


    trait Genome[A] {
        def cross(geno1: A, geno2: A): (A, A)
        def mutate(geno1: A): A
    }
    implicit class GenomeOps[A](a: A)(implicit ev: Genome[A]) {
        def cross(geno2: A): (A, A) = ev.cross(a, geno2)
        def mutate: A = ev.mutate(a)
    }


    case class Phenotype[A: Genome](
        genome: A,
        relativeFitness: Double, 
        trueFitness: Double,
        age: Int
    ){
        def reset: Phenotype[A] = 
            this.copy(relativeFitness = trueFitness)
    }


    case class Runner[A: Genome](
        val initPop: IndexedSeq[Phenotype[A]],
        val done: Population[A] => Boolean,
        val evolve: Population[A] => Population[A]
    ){
        
        def solve: (Population[A], List[(Double, Double)]) =
            run(Population(initPop, 0), List[(Double, Double)]())

        def run(p: Population[A], log: List[(Double, Double)] ): (Population[A], List[(Double, Double)]) = {
            val evolved = evolve(p)
            val nextPop = evolved.copy(adults = evolved.adults.map(_.reset))
            val finished = done(nextPop)
            val best = nextPop.fittest.trueFitness
            val avg = nextPop.averageFitness

            if(finished){
                println("Finished running")
                println(nextPop)
                (nextPop.copy(generation = nextPop.generation + 1), (avg, best) :: log)
            }
            else{
                println("generation %d".format(nextPop.generation))
                println(nextPop)
                run(nextPop.copy(generation = nextPop.generation + 1), (avg, best) :: log)
            }
        }
    }

    case class Population[A: Genome](
        val adults: IndexedSeq[Phenotype[A]],
        val generation: Int
    ){

        def fittest: Phenotype[A] =
            adults.reduceLeft( (l, r) => if (l.trueFitness > r.trueFitness) l else r)

        def averageFitness: Double =
            ((0.0 /: adults.map(_.trueFitness))(_+_))/adults.length


        def verbose: String = {
            "\n\nGeneration: " + generation +
            "\nAvg fitness: " +
            ((0.0 /: adults.map(_.trueFitness))(_+_))/adults.length +
            "\nBest fit \n: " +
            fittest +
            "\nPopulation --- \n" +
            adults.mkString("\n") + "\n\n"
        }


        override def toString: String = {
            "\n\nGeneration: " + generation +
            "\nAvg fitness: " +
            ((0.0 /: adults.map(_.trueFitness))(_+_))/adults.length +
            "\nBest fit \n: " +
            fittest +
            "\n individuals: %d\n".format(adults.length)
        }
   }
}
