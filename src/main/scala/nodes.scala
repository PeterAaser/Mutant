package scalapagos

import utils._
import scala.collection.mutable.ListBuffer

sealed trait Function

sealed trait SM 
sealed trait Input
sealed trait Output

case object NOP   extends Function
case object DADD  extends Function
case object DSUB  extends Function
case object CONST extends Function

case object INP     extends Function with Input
case object INPP    extends Function with Input
case object SKIPINP extends Function with Input
case object OUTPUT  extends Function with Output

case object ADD   extends Function with SM
case object DEL   extends Function with SM
case object MOV   extends Function with SM
case object OVR   extends Function with SM
case object DUP   extends Function with SM
case object FLUSH extends Function with SM


case class Node(c0: Int, c1: Int, p0: Double, p1: Double, p2: Double, f: Function)

class TODOList(size: Int, repr: ListBuffer[SM]){
  def append(n: SM): Unit = n match {
    case FLUSH => repr.clear
    case a: SM => repr.append(a)
  }
  def get: List[SM] = repr.reverse.take(size).toList
}
object TODOList {
  def apply(size: Int): TODOList = new TODOList(size, new ListBuffer[SM])
}


class InputList(repr: Array[Double]){
  var pointer = 0
  def inp: Double = {
    val r = repr(pointer)
    if(pointer >= repr.size) pointer = 0 else pointer += 1
    r
  }

  def inpp: Double = {
    val r = repr(pointer)
    if(pointer == 0) pointer = repr.size - 1 else pointer -= 1
    r
  }

  def inpskip(p: Double): Double = {
    val r = repr(pointer)
    pointer = (pointer + p.toInt) % repr.size
    r
  }
}

case class Graph(nodes: Array[Node]){

  val size           = nodes.size
  val known          = Array.fill(size)(false)
  val nodeValues     = Array.fill(size)(0.0)

  var inputPointer = 0

  def appendOutputs(outputsNeeded: Int): List[Int] = {
    var outputsLeft = outputsNeeded
    val traversalOrder = new ListBuffer[Int]
    for(ii <- 0 until size){
      nodes(ii).f match {
        case x: Function with Output if(outputsLeft > 0) => {
          outputsLeft -= 1
          traversalOrder.append(ii)
        }
      }
    }
    traversalOrder.toList.reverse
  }


  def appendNeeded(n: Int, traversalOrder: ListBuffer[Int]): Unit = {
    known(n) = true
    nodes(n).f match {
      case x: Function with Input => traversalOrder.append(n)
      case x: Function => {
        if(!known(nodes(n).c0)) appendNeeded(nodes(n).c0, traversalOrder)
        if(!known(nodes(n).c1)) appendNeeded(nodes(n).c1, traversalOrder)
        traversalOrder.append(n)
      }
    }
  }


  def getTraversalOrder: List[Int] = {
    val buf = new ListBuffer[Int]()
    val outputs = appendOutputs(2)
    outputs.foreach(n => appendNeeded(n, buf))
    buf.toList
  }


  def calculateValues(inputs: InputList): Unit = {
    getTraversalOrder.foreach{index => 
      val node = nodes(index)
      val c0 = index - node.c0
      val c1 = index - node.c1
      val nodeValue = node.f match {
        case x @ NOP     => nodeValues(c0)
        case x @ DADD    => nodeValues(c0) + nodeValues(c1)
        case x @ DSUB    => nodeValues(c0) - nodeValues(c1)
        case x @ CONST   => node.p0
               
        case x @ OUTPUT  => nodeValues(c0)
           
        case x @ INP     => inputs.inp
        case x @ INPP    => inputs.inpp
        case x @ SKIPINP => inputs.inpskip(node.p0)

        case x: Function with SM => nodeValues(c0)
      }
    }
  }


  // TODO: I don't like this way of collecting TODO nodes.
  // Needs verification
  def collectTodos: List[SM] = {
    val todo = TODOList(2)
    for(ii <- 0 until size){
      nodes(ii) match {
        case x: SM if known(ii) => todo.append(x)
      }
    }
    todo.get
  }
}


// object ShittyTest {

// }
