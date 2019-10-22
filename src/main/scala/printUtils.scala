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


  import Fungus._
  def printPetri(cells: Array[Array[GridContent]], food: Array[Array[Double]]): String = {
    val withFood = (cells zip food).map{ case(x, y) => x zip y }
    withFood.map(_.map{
      case(Cell(state, _), food) => {
        val cyto = state(1)
        val normalized = ((cyto/10.0).sqrtOr0*255.0).toInt.min(255)
        (fansi.Back.True(normalized, normalized, (food.toInt*20).min(255))("  ")).toString
      }
      case(Free, food) => (fansi.Back.True(0, 0, (food.toInt*20).min(255))("  ")).toString
      case(Wall, food) => (fansi.Back.True(255, 255, 255)("  ")).toString
    }.mkString).mkString("\n","\n","\n")
  }
}
