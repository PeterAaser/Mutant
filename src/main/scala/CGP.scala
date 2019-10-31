package scalapagos

import utils._
import IntBonusOps._
import DoubleBonusOps._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import cats._
import cats.Monoid
import scala.util.Try

import util.Random

class CGP(inputNodes: Int, outputNodes: Int, val nodes: Array[CGNode]) {

  def run(inputs: Array[Double]): Array[Double] = {

    for(ii <- 0 until inputNodes){
      nodes(ii).value = inputs(ii)
    }

    for(ii <- inputNodes until nodes.size){
      val node = nodes(ii)
      val arg0 = nodes(node.c0).value
      val arg1 = nodes(node.c1).value

      node.value = CGP.nodeFunctionLookup(node.f, arg0, arg1)
    }

    val output = Array.ofDim[Double](outputNodes)
    for(ii <- 0 until outputNodes){
      output(ii) = nodes((nodes.size + ii) - outputNodes).value
    }

    output
  }
}


object CGP {
  def apply(inputs: Int, outputs: Int, nodez: List[CGNode]): CGP = {

    val nodes = List.fill(inputs)(CGNode(0, 0, 0)) ++ nodez

    say(nodes.mkString("\n", "\n", "\n"))

    def appendNeeded(n: Int, buf: ArrayBuffer[Int]): Unit = {
      nodes(n).used = true
      n match {
        case x: Int if(x < inputs) => buf.append(n)
        case x: Int => {
          if(!nodes(n - nodes(n).c0).used) appendNeeded(n - nodes(n).c0, buf)
          if(!nodes(n - nodes(n).c1).used) appendNeeded(n - nodes(n).c1, buf)
          buf.append(n)
        }
      }
    }


    def calculateTraversalOrder: ArrayBuffer[Int] = {
      val traversalOrder = new ArrayBuffer[Int]()
      val outputIndexes = (0 until nodes.size).takeRight(outputs)
      outputIndexes.foreach(n => appendNeeded(n, traversalOrder))

      traversalOrder.sorted
      say(traversalOrder.sorted.toList)
      traversalOrder.sorted
    }


    /**
      * Removes all unecessary genes, hopefully improving memory behavior
      */
    def flatten(traversalOrder: ArrayBuffer[Int]): List[CGNode] = {
      traversalOrder
        .map(idx => (idx, nodes(idx)))
        .map{ case(idx, node) =>
          node.copy(c0 = idx - node.c0, c1 = idx - node.c1)
        }.toList
    }

    val traversalOrder = calculateTraversalOrder.sorted
    val flattened = flatten(traversalOrder)

    say(flattened.mkString("\n", "\n", "\n"))
    new CGP(inputs, outputs, flattened.toArray)
  }


  def nodeFunctionLookup(lookup: Int, arg0: Double, arg1: Double): Double = {

    val NOP        = 0
    val DADD       = 1
    val DSUB       = 2
    val CONST      = 3
    val DMULT      = 4
    val DDIV       = 5
    val AVG        = 6
    val DSQRT      = 7
    val DRCP       = 8
    val DABS       = 9
    val TANH       = 10
    val TANH2      = 11
    val POW        = 12
    val COS        = 13
    val SIN        = 14
    val MIN        = 15
    val MAX        = 16
    val IFLTE      = 17

    val out = lookup match {
      case NOP           => arg0
      case DADD          => arg0 + arg1
      case DSUB          => arg0 - arg1
      case DMULT         => arg0 * arg1
      case DDIV          => if(arg1 == 0.0) 0.0 else arg0/arg1
      case AVG           => (arg0 + arg1)/2.0
      case DSQRT         => if(arg0 > 0.0) math.sqrt(arg0) else 0.0
      case DRCP          => if(arg0 > 0.0) 1/math.sqrt(arg0) else 0.0
      case DABS          => math.abs(arg0)
      case TANH          => math.tanh(arg0)
      case TANH2         => math.tanh(arg0 + arg1)
      case POW           => math.pow(arg0, arg1)
      case COS           => math.cos(arg0)
      case SIN           => math.sin(arg0)
      case MIN           => math.min(arg0, arg1)
      case MAX           => math.max(arg0, arg1)
      case IFLTE         => if(arg0 > arg1) 1.0 else 0.0

      case _             => say("bad function arg"); arg0
    }

    if(out.isNaN || out.isInfinity){
      0.0
    }
    else
      out
  }

  val ghettoTest = {

    val hur = CGenetics.generate(
      inputNodes  = 5,
      outputNodes = 4,
      layers      = 5,
      layerSize   = 3,
      lookBack    = 2
    )
    say(hur.toList.mkString("\n","\n","\n"))
    say(hur.slice(5, 20).grouped(3).map(_.mkString("\n","\n\n","\n\n")).toList)

    // CGP(
    //   6, 3,
    //   List(
    //     CGNode(4, 6, 1),
    //     CGNode(6, 2, 2),
    //     CGNode(5, 5, 3),
        
    //     CGNode(9, 2, 4),
    //     CGNode(4, 3, 5),
    //     CGNode(8, 6, 6),
        
    //     CGNode(6, 2, 7),
    //     CGNode(2, 7, 8),
    //     CGNode(5, 4, 9),
        
    //     CGNode(6, 2, 10),
    //     CGNode(7, 7, 11),
    //     CGNode(4, 4, 12),
    //   )
    // )

  }
}
