package scalapagos

import utils._
import scala.collection.mutable.ListBuffer
import cats._
import cats.Monoid
import scala.util.Try

import util.Random

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
case object DU2   extends Function with SM
case object DU3   extends Function with SM
case object DU4   extends Function with SM
case object FLUSH extends Function with SM

case object COPYTOSTOP extends Function with SM
case object COPYSTOP   extends Function with SM

case object SHIFTCONN   extends Function with SM
case object SHIFTCONN2  extends Function with SM

case object CHC extends Function with SM
case object CHF extends Function with SM
case object CHP extends Function with SM

case class Node(c0: Int, c1: Int, p0: Double, p1: Double, p2: Double, f: Function){
  var value = 0.0
  var known = false
  def reset: Unit = {known = false}
}

object Node {

  val lookupTable = Array(
    NOP,
    DADD,
    DSUB,
    CONST,

    INP,
    INPP,
    SKIPINP,
    OUTPUT,

    ADD,
    DEL,
    MOV,
    OVR,
    DUP,
    DU2,
    DU3,
    DU4,
    FLUSH,
    COPYTOSTOP,
    COPYSTOP,
    SHIFTCONN,
    SHIFTCONN2,
    CHC,
    CHF,
    CHP,
  )

  val lookupReverse: Map[Function, Int] = lookupTable.toList.zipWithIndex.toMap

  def randomFunction = lookupTable(Random.nextInt(lookupTable.size - 1))

  def randomNode: Node = {
    Node(
      Random.nextInt(5),
      Random.nextInt(5),

      (5.0 * Random.nextDouble) - 2.5,
      (5.0 * Random.nextDouble) - 2.5,
      (5.0 * Random.nextDouble) - 2.5,

      randomFunction
    )
  }
}

class TODOList(size: Int, repr: ListBuffer[(SM, Int)]){
  def append(n: SM, idx: Int): Unit = n match {
    case FLUSH => repr.clear
    case a: SM => repr.append((a, idx))
  }
  def get: List[(SM, Int)] = repr.reverse.take(size).toList
}
object TODOList {
  def apply(size: Int): TODOList = new TODOList(size, new ListBuffer[(SM, Int)])
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

  val size = nodes.size
  def getNode(n: Int): Node = if(n < 0) nodes(0) else nodes(n)
  def clamp(n: Int): Int = if(n < 0) 0 else if(n >= size) size - 1 else n

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
        case _ => ()
      }
    }
    traversalOrder.toList
  }


  def appendNeeded(n: Int, traversalOrder: ListBuffer[Int]): Unit = {
    nodes(n).known = true
    nodes(n).f match {
      case x: Function with Input => traversalOrder.append(n)
      case x: Function => {
        if(!getNode(n - nodes(n).c0).known) appendNeeded(n - nodes(n).c0, traversalOrder)
        if(!getNode(n - nodes(n).c1).known) appendNeeded(n - nodes(n).c1, traversalOrder)
        traversalOrder.append(n)
      }
    }
    say(s"traversal order: ${traversalOrder.toList}")
  }


  def getTraversalOrder: List[Int] = {
    val buf = new ListBuffer[Int]()
    val outputs = appendOutputs(2)
    say(outputs)
    outputs.foreach(n => appendNeeded(n, buf))
    buf.toList
  }


  def calculateValues(inputs: InputList): Unit = {
    getTraversalOrder.foreach{index =>
      val node = nodes(index)
      val c0 = index - node.c0
      val c1 = index - node.c1
      val nodeValue = node.f match {
        case x @ NOP     => getNode(c0).value
        case x @ DADD    => getNode(c0).value + getNode(c1).value
        case x @ DSUB    => getNode(c0).value - getNode(c1).value
        case x @ CONST   => node.p0

        case x @ OUTPUT  => getNode(c0).value

        case x @ INP     => inputs.inp
        case x @ INPP    => inputs.inpp
        case x @ SKIPINP => inputs.inpskip(node.p0)

        case x: Function with SM => getNode(c0).value
      }
      nodes(index).value = nodeValue
    }
  }


  // TODO: I don't like this way of collecting TODO nodes.
  // Needs verification
  def collectTodos: List[(SM, Int)] = {
    val todo = TODOList(2)
    for(ii <- 0 until size){
      nodes(ii).f match {
        case x: Function with SM if nodes(ii).known => {
          if(getNode(ii - nodes(ii).c0).value >= getNode(ii - nodes(ii).c1).value)
            todo.append(x, ii)
        }
        case _ => ()
      }
    }
    todo.get
  }
}


object ShittyTest {

  def test: Unit = {
    val nodes = List(
      Node(1, 1, 0.0, 0.0, 0.0, INP),
      Node(1, 1, 0.0, 0.0, 0.0, INP),

      Node(1, 1, 0.0, 0.0, 0.0, NOP),
      Node(1, 1, 0.0, 0.0, 0.0, NOP),

      Node(3, 4, 0.0, 0.0, 0.0, ADD),

      Node(3, 4, 0.0, 0.0, 0.0, NOP),

      Node(2, 5, 0.0, 0.0, 0.0, DSUB),
      Node(3, 3, -1.0, 0.0, 0.0, CONST),

      Node(3, 3, 1.0, 0.0, 0.0, NOP),

      Node(3, 8, 1.0, 0.0, 0.0, OUTPUT),
      Node(1, 3, 1.0, 0.0, 0.0, OUTPUT),
    )

    val graph = Graph(nodes.toArray)

    graph.calculateValues(new InputList(Array(2.0, 1.0)))
    say(graph.collectTodos.toList)
  }
}
