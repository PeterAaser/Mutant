package scalapagos

import utils._
import IntBonusOps._

import scala.collection.mutable.ListBuffer
import cats._
import cats.Monoid
import scala.util.Try

import util.Random

sealed trait Function

sealed trait SM
sealed trait Input
sealed trait Output

object NodeLookup {
  val INP        = 0
  val NOP        = 1
  val DADD       = 2
  val DSUB       = 3
  val CONST      = 4
  val DMULT      = 5
  val DDIV       = 6
  val AVG        = 7
  val DSQRT      = 8
  val DRCP       = 9
  val DABS       = 10
  val TANH       = 11
  val TANH2      = 12
  val POW        = 13
  val COS        = 14
  val SIN        = 15
  val MIN        = 16
  val MAX        = 17
  val IFLTE      = 18
  val INDX       = 19
  val OUTPUT     = 20
  val DEL        = 21
  val MOV        = 22
  val OVR        = 23
  val DUP        = 24
  val DU2        = 25
  val DU3        = 26
  val DU4        = 27
  val FLUSH      = 28
  val COPYTOSTOP = 29
  val COPYSTOP   = 30
  val SHIFTCONN  = 31
  val SHIFTCONN2 = 32
  val CHC        = 33
  val CHF        = 34
  val CHP        = 35
}


case class Node(c0: Int, c1: Int, p0: Double, p1: Double, p2: Double, f: Int){
  var value = 0.0
  var known = false
  def reset: Unit = {known = false}

  override def toString = {
    if(known)
      Console.GREEN ++ s"$f\t[$c0, $c1]\t" ++ f"$value%1.2f" ++ Console.RESET
    else
      Console.RED   ++ s"$f\t[$c0, $c1]\t" ++ f"$value%1.2f" ++ Console.RESET
  }
}
object Node {
  def randomNode = Node(
    util.Random.nextInt(5) + 1,
    util.Random.nextInt(5) + 1,
    (5.0 - (10.0 * util.Random.nextDouble)),
    (5.0 - (10.0 * util.Random.nextDouble)),
    (5.0 - (10.0 * util.Random.nextDouble)),
    util.Random.nextInt(35)
  )
}
