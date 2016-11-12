package GA

import scala._
import scalaz._
import Scalaz._

import Data._
import scala.util.Random

import Representations._
import Scaling._
import Reproduction._
import ParentSelection._



import reflect.runtime.universe._

// object OneMax {
// 
//     def randomEvaluator(l: Int): (SingleBitGenome => Double) = {
//         val ideal = Vector.fill(l)(Random.nextInt(2))
//         genome => {
//             val hits = (genome.gene.bits zip ideal).map(t => if (t._1 == t._2) 1 else 0)
//             hits.sum.toDouble
//         }
//     }
// 
//     val evaluate: (SingleBitGenome => Double) = ???
//         // genome => {
//         //     val sum = (0 /: genome.gene.bits)(_+_)
//         //     sum.toDouble
//         // }
// }
// 
// 
// object LOLZ {
// 
//     def count(v: Vector[Int], target: Int): Int = {
//         v match {
//             case h +: t if(h == target) => 1 + count(t, target)
//             case _ => 0
//         }
//     }
// 
//     def evaluator(z: Int): (SingleBitGenome => Double) = {
//             // genome: SingleBitGenome => {
//             //     val leading = count(genome.gene.bits, genome.gene.bits.head)
//             //     if(genome.gene.bits.head == 0)
//             //         if (leading > z) z.toDouble else leading.toDouble
//             //     else
//             //         leading.toDouble
//             // }
//             ???
//         }
// }
// 
// object Suprising {
// 
//     import scala.collection.mutable.HashSet
//     // import scala.collection.optimizer._
// 
//     case class SubSeq(s1: Int, s2: Int, d: Int)
// 
//     // I think this caches
//     def maxUnique(length: Int, distance: Int): Int = {
//         val d = if(distance > length) length else distance 
//         d*( (length - 1) - d) + (d*(d + 1))/2
//     }
// 
//     def collect(h: SymbolGene, t: IndexedSeq[SymbolGene], d: Int, found: HashSet[SubSeq]) = {
//         val successors = (t take d).zipWithIndex
//         successors.toList.foreach( { case (s, i) => found += SubSeq(h.symbol, s.symbol, i) } )
//     }
// 
// 
//     def evaluator(s: Int): (SymbolGenome => Double) = {
//         def collectAll(g: IndexedSeq[SymbolGene], found: HashSet[SubSeq]): Unit = { 
//             g match {
//                 case h +: t => collect(h, t, s, found); collectAll(t, found)
//                 case _ => ()
//             }
//         }
//         val found = HashSet[SubSeq]()
//         candidate => {
//             collectAll(candidate.genes, found)
//             found.size.toDouble/(maxUnique(candidate.genes.length, s))
//         }
//     }
// }
