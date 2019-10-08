package scalapagos

import utils._
import IntBonusOps._
import DoubleBonusOps._

import scala.collection.mutable.ListBuffer
import cats._
import cats.Monoid
import scala.util.Try

import util.Random


abstract class SMCG(numOutputs: Int = 5) {

  var nodes: Array[Node]
  def size = nodes.size
  def getNode(n: Int): Node = if(n < 0) nodes(0) else nodes(n)
  def clamp(n: Int): Int = if(n < 0) 0 else if(n >= size) size - 1 else n

  def isSM(f: Int): Boolean
  def isInput(f: Int): Boolean
  def isOutput(f: Int): Boolean
  def isFlush(f: Int): Boolean

  def nodeFunctionLookup(lookup: Int, arg0: Double, arg1: Double, p0: Double, idx: Int, input: Array[Double]): Double
  def applySM(idx: Int): Boolean

  def appendOutputs: List[Int] = {
    val traversalOrder = new ListBuffer[Int]
    for(ii <- 0 until size){
      if(isOutput(nodes(ii).f))
        traversalOrder.append(ii)
    }
    traversalOrder.toList
  }


  def appendNeeded(n: Int, traversalOrder: ListBuffer[Int]): Unit = {
    nodes(n).known = true
    nodes(n).f match {
      case x: Int if(isInput(x)) => traversalOrder.append(n)
      case x: Int => {
        if(!getNode(n - nodes(n).c0).known) appendNeeded(clamp(n - nodes(n).c0), traversalOrder)
        if(!getNode(n - nodes(n).c1).known) appendNeeded(clamp(n - nodes(n).c1), traversalOrder)
        traversalOrder.append(n)
      }
    }
  }




  /**
    * Calculates all necessary node values.
    * For traditional SMCGP an input list must be used, thus this thing must be overriden.
    * I dunno, I'm not good at OOP
    */
  def calculateValues(inputs: Array[Double]): Array[Double] = {

    getTraversalOrder.foreach{ index =>
      val node = nodes(index)
      val c0 = index - node.c0
      val c1 = index - node.c1

      val p0 = nodes(index).p0

      val arg0 = getNode(c0).value
      val arg1 = getNode(c1).value

      nodes(index).value = nodeFunctionLookup(nodes(index).f, arg0, arg1, p0, index, inputs)
    }

    val outBuf = Array.ofDim[Double](numOutputs)
    nodes.filter{ node => isOutput(node.f) }.foreach{ node =>
      val idx = node.p0.floorInt.modp(numOutputs - 1)
      outBuf(idx) += node.value
    }
    outBuf
  }


  /**
    * When an SM node is called, the process changes slightly. If an SM node is ‘activated’;
    * then its self-modification instructions are added to a list of pending manipulations
    * which is called the To-Do list.
    * The modifications in this list are then performed between iterations.
    * Note that SM active instructions are added to the To-Do list in a left-to-right traversal
    * of the encoded graph.
    * It was decided that SM nodes should be activated in a way that is dependent on the data presented to them.
    * 
    * Return is a list of indexes
    */
  def collectSM: Vector[Int] = {
    val todo = ListBuffer[Int]()
    for(ii <- 0 until size){
      if(nodes(ii).known && isSM(nodes(ii).f)){
        if(getNode(ii - nodes(ii).c0).value >= getNode(ii - nodes(ii).c1).value){
          if(isFlush(nodes(ii).f))
            todo.clear
          else
            todo.append(ii)
        }
      }
    }
    todo.toVector
  }


  def applyTodos: Boolean = {
    val todoList = collectSM
    var done = false
    var idx = 0
    // say(s"applying TODO items: $todoList")
    while((!done) && todoList.isDefinedAt(idx)){
      done = applySM(todoList(idx))
      idx  = idx + 1
    }
    done
  }


  def getTraversalOrder: List[Int] = {
    val buf = new ListBuffer[Int]()
    val outputs = appendOutputs
    outputs.foreach(n => appendNeeded(n, buf))
    // dsay(buf.toList)
    buf.toList
  }


  def run(input: Array[Double]): (Boolean, Array[Double]) = {
    val output = calculateValues(input)
    // say(nodes.toList.mkString("\n","\n", "\n"))
    val changed = applyTodos
    // if(!changed)
    //   say("No changes were done")
    nodes.foreach(_.reset)
    (changed, output.toArray)
  }
}
