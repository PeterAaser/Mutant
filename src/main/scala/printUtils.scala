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


  def printPetri(cells: Array[Array[Option[Array[Double]]]], food: Array[Array[Double]]): String = {
    val cyto = cells.map(_.map(_.map(x => x(2)).getOrElse(0.0)))
    val withFood = (cyto zip food).map{ case(x, y) => x zip y }
    withFood.map(_.map{ case(x, y) =>
      val normalized = ((x/10.0).sqrtOr0*255.0).toInt.min(255)
      if((x == 0.0) && (y == 0.0))
        ".."
      else
        (fansi.Back.True(normalized, normalized, (y.toInt*20).min(255))("  ")).toString
    }.mkString).mkString("\n","\n","\n")
  }

}
