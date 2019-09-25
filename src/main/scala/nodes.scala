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

  override def toString = {
    if(known)
      Console.GREEN ++ s"$f\t[$c0, $c1]\t" ++ f"$value%1.2f" ++ Console.RESET
    else
      Console.RED ++ s"[$f, $c0, $c1]" ++ Console.RESET
  }
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

class TODOList(size: Int, repr: ListBuffer[(Node, Int)]){
  def append(n: Node, idx: Int): Unit = n.f match {
    case FLUSH => repr.clear
    case a: SM => repr.append((n, idx))
  }
  def get: List[(Node, Int)] = repr.reverse.take(size).toList
}
object TODOList {
  def apply(size: Int): TODOList = new TODOList(size, new ListBuffer[(Node, Int)])
}


class InputList(repr: Array[Double]){
  var pointer = 0
  def inp: Double = {
    val r = repr(pointer)
    if(pointer >= (repr.size - 1)) pointer = 0 else pointer += 1
    r
  }

  def inpp: Double = {
    val r = repr(pointer)
    if(pointer == 0) pointer = repr.size - 1 else pointer -= 1
    r
  }

  def inpskip(p: Double): Double = {
    val r = repr(pointer)
    pointer = (pointer + p.toInt).modp(repr.size - 1)
    r
  }
}

case class Graph(var nodes: Array[Node]) extends Modifiers {

  def size = nodes.size
  def getNode(n: Int): Node = if(n < 0) nodes(0) else nodes(n)

  def appendOutputs: List[Int] = {
    val traversalOrder = new ListBuffer[Int]
    for(ii <- 0 until size){
      nodes(ii).f match {
        case x: Function with Output => {
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
        if(!getNode(n - nodes(n).c0).known) appendNeeded(clamp(n - nodes(n).c0), traversalOrder)
        if(!getNode(n - nodes(n).c1).known) appendNeeded(clamp(n - nodes(n).c1), traversalOrder)
        traversalOrder.append(n)
      }
    }
  }


  def getTraversalOrder: List[Int] = {
    val buf = new ListBuffer[Int]()
    val outputs = appendOutputs
    outputs.foreach(n => appendNeeded(n, buf))
    buf.toList
  }


  def calculateValues(inputs: InputList): List[Double] = {
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
    nodes.filter{ n => n.f match {
      case x @ OUTPUT => true
      case _ => false
    }}.map(_.value).toList
  }


  // TODO: I don't like this way of collecting TODO nodes.
  // Needs verification
  def collectTodos: List[(Node, Int)] = {
    val todo = TODOList(2)
    for(ii <- 0 until size){
      nodes(ii).f match {
        case x: Function with SM if nodes(ii).known => {
          if(getNode(ii - nodes(ii).c0).value >= getNode(ii - nodes(ii).c1).value)
            todo.append(nodes(ii), ii)
        }
        case _ => ()
      }
    }
    todo.get
  }


  def applyTodos: Boolean = {
    val todoList = collectTodos
    var done = false
    var idx = 0
    while((!done) && todoList.isDefinedAt(idx)){
      val (next, success) = Function.tupled(modify _)(todoList(idx))
      done = success
      idx = idx + 1
      if(done)
        nodes = next
    }
    done
  }

  /**
    Runs the network, performs modifications, resets nodes and
    returns the output
    */
  def run(input: List[Double]): (Boolean, List[Double]) = {
    val output = calculateValues(new InputList(input.toArray))
    say(nodes.toList.mkString("\n","\n", "\n"))
    val changed = applyTodos
    nodes.foreach(_.reset)
    (changed, output)
  }

}


object ShittyTest {

  def test: Unit = {
    val nodes = List(
      Node(1,  1,   1.0,   0.0,   0.0, NOP),
      Node(1,  1,   1.0,   0.0,   0.0, INP),

      Node(4,  1,   3.3,   0.0,   0.0, NOP),
      Node(1,  1,   2.0,   0.0,   0.0, NOP),

      //   c1  c2   p0     p1     p2
      Node(3,  7,   2.6,   1.3,   -8.0, DUP),

      Node(3,  4,   0.0,   0.0,   0.0, NOP),

      Node(2,  5,   0.0,   0.0,   0.0, DSUB),
      Node(3,  3,  -1.0,   0.0,   0.0, CONST),

      Node(3,  3,   1.0,   0.0,   0.0, NOP),

      Node(3,  8,   1.0,   0.0,   0.0, OUTPUT),
      Node(1,  3,   1.0,   0.0,   0.0, OUTPUT),
    )


    val graph = Graph(nodes.toArray)

    for(ii <- 0 until 5){ graph.run(List(2.0, 1.0)) }
  }
}
