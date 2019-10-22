package scalapagos

import utils._
import IntBonusOps._

import scala.collection.mutable.ListBuffer
import cats._
import cats.Monoid
import scala.util.Try

import util.Random

abstract class CellControl(var nodes: Array[Node]) extends SMCG with Modifiers{
  override def clamp(n: Int): Int = if(n < 0) 0 else if(n >= size) size - 1 else n
}

object CellControl {
  def apply(nodez: Array[Node]): CellControl = new CellControl(nodez) {


    override def isSM(f: Int)     : Boolean = f > 20
    override def isInput(f: Int)  : Boolean = f == 1
    override def isOutput(f: Int) : Boolean = f == 20
    override def isFlush(f: Int)  : Boolean = f == 28

    override def nodeFunctionLookup(lookup: Int, arg0: Double, arg1: Double, p0: Double, idx: Int, input: Array[Double]): Double = {
      import NodeLookup._

      val out = lookup match {
        case NOP           => arg0
        case DADD          => arg0 + arg1
        case DSUB          => arg0 - arg1
        case CONST         => p0
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
        case INDX          => idx.toDouble
        case OUTPUT        => arg0
        case INP           => { input(p0.toInt.modp(input.size - 1)) }

        case x if(isSM(x)) => arg0
        case _             => arg0
      }

      if(out.isNaN || out.isInfinity){
        0.0
      }
      else
        out
    }


    override def applySM(idx: Int): Boolean = modify(idx)
  }

  def random(size: Int) = {
    val nodes = Array.fill(size)(Node.randomNode)
    CellControl(nodes)
  }
}
