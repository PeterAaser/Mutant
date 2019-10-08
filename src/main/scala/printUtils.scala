package scalapagos

import utils._
import IntBonusOps._
import DoubleBonusOps._
import cats._
import cats.implicits._

import fansi._

object PrintUtils {

  def cytoHeat(concentration: Double): String = {
    val normalized = ((concentration/10.0).sqrtOr0*255.0).toInt.min(255)
    (fansi.Back.True(normalized, normalized, 0)("  ")).toString
  }

  // debug only
  def cytoHeat2(concentration: Double): String = {
    val str = "%.2f".format(concentration)
    s"[$str]"
    // (fansi.Back.True(normalized, normalized, 0)(" ")).toString
  }

  def printSlimeMold(cells: Array[Array[Option[Array[Double]]]]): String = {
    cells.map(_.map(_.map{x =>
      cytoHeat(x(2))
    }.getOrElse("..")).mkString).mkString("\n","\n","\n")
  }

  def printFood(cells: Array[Array[Double]]): String = {
    cells.map(_.map{x =>
      cytoHeat(x)
    }.mkString).mkString("\n","\n","\n")
  }

  def printSlimeMoldSignals(cells: Array[Array[Option[Array[Double]]]]): String = {
    cells.map(_.map(_.map{x =>
      cytoHeat(x(3))
    }.getOrElse(".")).mkString).mkString("\n","\n","\n")
  }

}
