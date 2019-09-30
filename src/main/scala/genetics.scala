package scalapagos

import util.Random
import IntBonusOps._

object Genetics {

  // For now let's just prepend an inp and postpend(?) an output
  def generateRandom(implicit params: Params): Graph = {
    val randomNodes = Array.fill[Node](params.initNodes + 2)(Node.randomNode)
    randomNodes(0) = randomNodes(0).copy(f = INP)
    randomNodes(randomNodes.size - 1) = randomNodes(randomNodes.size - 1).copy(f = OUTPUT)
    new Graph(randomNodes)
  }

  def mutateNode(graph: Graph)(implicit params: Params): Unit = {
    val repr                  = graph.nodes
    val idx                   = Random.nextInt(repr.size - 1)
    val modificationAttribute = Random.nextInt(4)
    import params._

    def mutateConnection(c: Int): Int = {
      val next = c + Random.nextInt(connectionMutationMax) - (connectionMutationMax/2)
      next.min1
    }

    def mutateParam(d: Double): Double = (1.0 - Random.nextDouble) * paramMutationMax

    repr(idx) = modificationAttribute match {
      case 0 => repr(idx).copy(c0 = mutateConnection(repr(idx).c0))
      case 1 => repr(idx).copy(c1 = mutateConnection(repr(idx).c1))
      case 2 => repr(idx).copy(p0 = mutateParam(repr(idx).p0))
      case 3 => repr(idx).copy(p1 = mutateParam(repr(idx).p1))
      case 4 => repr(idx).copy(p2 = mutateParam(repr(idx).p2))
      case 5 => repr(idx).copy(f = Node.randomFunction)
    }
  }
}
