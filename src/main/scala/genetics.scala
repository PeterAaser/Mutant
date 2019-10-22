package scalapagos

import IntBonusOps._

/** Only rudimentary mutation supported atm */
object Genetics {

  /**
    * Mutates a genome. Probability of mutation is given as the mutationRate parameter in GAParams
    */
  def mutate(nodes: Array[Node])(implicit p: GAParams): Array[Node] = {

    import util.Random._

    def addNode(node: Node): Array[Node] = {
      Array(node, Node.randomNode)
    }

    def mutateConnection(node: Node): Array[Node] = {
      if(nextDouble() > 0.5)
        Array(node.copy(c0 = (node.c0 + (5 - nextInt(10))).min1))
      else
        Array(node.copy(c1 = (node.c1 + (5 - nextInt(10))).min1))
    }

    def mutateParam(node: Node): Array[Node] = {
      val check = nextDouble()
      if(check > 0.33)
        Array(node.copy(p0 = (node.p0 + (5.0 - 10.0*nextDouble()))))
      else if(check > 0.66)
        Array(node.copy(p1 = (node.p1 + (5.0 - 10.0*nextDouble()))))
      else
        Array(node.copy(p2 = (node.p2 + (5.0 - 10.0*nextDouble()))))
    }

    def mutateFunction(node: Node): Array[Node] = {
      Array(node.copy(f = nextInt(NodeLookup.names.toList.size)))
    }

    def removeNode(node: Node): Array[Node] = Array[Node]()

    def mutate(node: Node): Array[Node] = {
      shuffle(List(mutateConnection(_), mutateParam(_), mutateFunction(_), removeNode(_))).head(node)
    }

    nodes.flatMap{ node =>
      if(util.Random.nextDouble() < p.mutationRate){
        mutate(node)
      }
      else
        Array(node)
    }

  }
}
