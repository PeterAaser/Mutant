package scalapagos

import utils._
import IntBonusOps._
import DoubleBonusOps._
import cats._
import cats.implicits._
import collection.mutable.ArrayBuffer

object GridOps {
  
  implicit class gridOps(private val grid: Array[Array[GridContent]]) extends AnyVal {

    def freeNeighbours(ii: Int)(jj: Int): List[(Int, Int)] = {
      val buf = new ArrayBuffer[(Int, Int)](5)
      if(grid.isDefinedAt(ii - 1) && grid(ii - 1).isDefinedAt(jj + 1)) grid(ii - 1)(jj + 1) match{ case Free => buf.append((ii - 1, jj + 1)) ; case _ => ()}
      if(grid.isDefinedAt(ii    ) && grid(ii    ).isDefinedAt(jj + 1)) grid(ii    )(jj + 1) match{ case Free => buf.append((ii    , jj + 1)) ; case _ => ()}
      if(grid.isDefinedAt(ii + 1) && grid(ii + 1).isDefinedAt(jj + 1)) grid(ii + 1)(jj + 1) match{ case Free => buf.append((ii + 1, jj + 1)) ; case _ => ()}

      if(grid.isDefinedAt(ii - 1) && grid(ii - 1).isDefinedAt(jj    )) grid(ii - 1)(jj    ) match{ case Free => buf.append((ii - 1, jj    )) ; case _ => ()}
      if(grid.isDefinedAt(ii + 1) && grid(ii + 1).isDefinedAt(jj    )) grid(ii + 1)(jj    ) match{ case Free => buf.append((ii + 1, jj    )) ; case _ => ()}

      if(grid.isDefinedAt(ii - 1) && grid(ii - 1).isDefinedAt(jj - 1)) grid(ii - 1)(jj - 1) match{ case Free => buf.append((ii - 1, jj - 1)) ; case _ => ()}
      if(grid.isDefinedAt(ii    ) && grid(ii    ).isDefinedAt(jj - 1)) grid(ii    )(jj - 1) match{ case Free => buf.append((ii    , jj - 1)) ; case _ => ()}
      if(grid.isDefinedAt(ii + 1) && grid(ii + 1).isDefinedAt(jj - 1)) grid(ii + 1)(jj - 1) match{ case Free => buf.append((ii + 1, jj - 1)) ; case _ => ()}

      buf.toList
    }
    
    def neighborCells(ii: Int)(jj: Int): List[Cell] = {
      val buf = new ArrayBuffer[Cell](5)
      if(grid.isDefinedAt(ii - 1) && grid(ii - 1).isDefinedAt(jj + 1)) grid(ii - 1)(jj + 1) match{ case x: Cell => buf.append(x) ; case _ => ()}
      if(grid.isDefinedAt(ii    ) && grid(ii    ).isDefinedAt(jj + 1)) grid(ii    )(jj + 1) match{ case x: Cell => buf.append(x) ; case _ => ()}
      if(grid.isDefinedAt(ii + 1) && grid(ii + 1).isDefinedAt(jj + 1)) grid(ii + 1)(jj + 1) match{ case x: Cell => buf.append(x) ; case _ => ()}

      if(grid.isDefinedAt(ii - 1) && grid(ii - 1).isDefinedAt(jj    )) grid(ii - 1)(jj    ) match{ case x: Cell => buf.append(x) ; case _ => ()}
      if(grid.isDefinedAt(ii + 1) && grid(ii + 1).isDefinedAt(jj    )) grid(ii + 1)(jj    ) match{ case x: Cell => buf.append(x) ; case _ => ()}

      if(grid.isDefinedAt(ii - 1) && grid(ii - 1).isDefinedAt(jj - 1)) grid(ii - 1)(jj - 1) match{ case x: Cell => buf.append(x) ; case _ => ()}
      if(grid.isDefinedAt(ii    ) && grid(ii    ).isDefinedAt(jj - 1)) grid(ii    )(jj - 1) match{ case x: Cell => buf.append(x) ; case _ => ()}
      if(grid.isDefinedAt(ii + 1) && grid(ii + 1).isDefinedAt(jj - 1)) grid(ii + 1)(jj - 1) match{ case x: Cell => buf.append(x) ; case _ => ()}

      buf.toList
    }

    def foreachNeighbours(ii: Int)(jj: Int)(fta: Cell => Unit): Unit = {
      if(grid.isDefinedAt(ii - 1) && grid(ii - 1).isDefinedAt(jj + 1)) grid(ii - 1)( jj + 1) match { case x: Cell => fta(x) ; case _ => ()}
      if(grid.isDefinedAt(ii    ) && grid(ii    ).isDefinedAt(jj + 1)) grid(ii    )( jj + 1) match { case x: Cell => fta(x) ; case _ => ()}
      if(grid.isDefinedAt(ii + 1) && grid(ii + 1).isDefinedAt(jj + 1)) grid(ii + 1)( jj + 1) match { case x: Cell => fta(x) ; case _ => ()}
                                                                                                                          
      if(grid.isDefinedAt(ii - 1) && grid(ii - 1).isDefinedAt(jj    )) grid(ii - 1)( jj    ) match { case x: Cell => fta(x) ; case _ => ()}
      if(grid.isDefinedAt(ii + 1) && grid(ii + 1).isDefinedAt(jj    )) grid(ii + 1)( jj    ) match { case x: Cell => fta(x) ; case _ => ()}
                                                                                                                          
      if(grid.isDefinedAt(ii - 1) && grid(ii - 1).isDefinedAt(jj - 1)) grid(ii - 1)( jj - 1) match { case x: Cell => fta(x) ; case _ => ()}
      if(grid.isDefinedAt(ii    ) && grid(ii    ).isDefinedAt(jj - 1)) grid(ii    )( jj - 1) match { case x: Cell => fta(x) ; case _ => ()}
      if(grid.isDefinedAt(ii + 1) && grid(ii + 1).isDefinedAt(jj - 1)) grid(ii + 1)( jj - 1) match { case x: Cell => fta(x) ; case _ => ()}
    }

    def isSurrounded(ii: Int)(jj: Int): Boolean = {
      val free = freeNeighbours(ii)(jj)
      free.size == 0
    }
  }
}
