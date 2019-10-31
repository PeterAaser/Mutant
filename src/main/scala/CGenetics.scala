package scalapagos

import utils._
import IntBonusOps._
import DoubleBonusOps._

import util.Random

object CGenetics {

  def generate(inputNodes: Int, outputNodes: Int, layers: Int, layerSize: Int, lookBack: Int): List[CGNode] = {

    val nodes = Array.ofDim[CGNode](inputNodes + (layers*layerSize) + outputNodes)

    // Generate NOP input nodes
    for(ii <- 0 until inputNodes){
      // say(s"input node: $ii")
      nodes(ii) = CGNode(0, 0, 0)
    }

    // Generate input connected nodes
    for(ii <- 0 until layerSize * lookBack){
      // say(s"input connected node: ${ii + inputNodes}")
      val f = Random.nextInt(17)

      val currentDepth = ii%layerSize

      val minConnect = 1 + currentDepth
      val maxConnect = ii + inputNodes

      val c0 = Random.between(minConnect, maxConnect + 1)
      val c1 = Random.between(minConnect, maxConnect + 1)

      nodes(inputNodes + ii) = CGNode(c0, c1, f)
    }


    // Generate internal layers
    for(ii <- (lookBack*layerSize) + inputNodes until (layers*layerSize) + inputNodes){
      val f = Random.nextInt(17)

      val currentDepth = (ii - inputNodes)%layerSize

      val minConnect = 1 + currentDepth
      val maxConnect = lookBack*layerSize + currentDepth

      // say(s"internal node: $ii")
      // say(s"current depth: $currentDepth")
      // say(s"min connect: $minConnect")
      // say(s"max connect: $maxConnect\n")

      val c0 = Random.between(minConnect, maxConnect + 1)
      val c1 = Random.between(minConnect, maxConnect + 1)

      nodes(ii) = CGNode(c0, c1, f)
    }


    // Generate output layer
    for(ii <- (inputNodes + (layers * layerSize)) until (inputNodes + (layers * layerSize) + outputNodes)){
      val f = Random.nextInt(17)

      val currentDepth = (ii - (inputNodes + (layers * layerSize))) % outputNodes

      val minConnect = 1 + currentDepth
      val maxConnect = lookBack*layerSize + currentDepth

      val c0 = Random.between(minConnect, maxConnect + 1)
      val c1 = Random.between(minConnect, maxConnect + 1)

      nodes(ii) = CGNode(c0, c1, f)
    }

    nodes.toList
  }


  def repair(inputNodes: Int, outputNodes: Int, layers: Int, layerSize: Int, lookBack: Int)(index: Int, nodes: Array[CGNode]): Unit = {

    def repairInputConnected = {
      say(s"repairing IC connected")
      val node = nodes(inputNodes + index)

      val currentDepth = index % layerSize
      val minConnect = 1 + currentDepth
      val maxConnect = index + inputNodes

      nodes(inputNodes + index) = node.copy(
        c0 = node.c0.constrain(minConnect, maxConnect),
        c1 = node.c1.constrain(minConnect, maxConnect)
      )
    }


    def repairInternal = {
      say(s"repairing internal")
      val node = nodes(index)

      val currentDepth = (index - inputNodes)%layerSize
      val minConnect = 1 + currentDepth
      val maxConnect = lookBack*layerSize + currentDepth

      say(currentDepth)
      say(minConnect)
      say(maxConnect)

      nodes(index) = node.copy(
        c0 = node.c0.constrain(minConnect, maxConnect),
        c1 = node.c1.constrain(minConnect, maxConnect)
      )
    }


    def repairOutput = {
      say(s"repairing output")
      val node = nodes(index)

      val currentDepth = (index - (inputNodes + (layers * layerSize))) % outputNodes
      val minConnect = 1 + currentDepth
      val maxConnect = lookBack*layerSize + currentDepth

      say(currentDepth)
      say(minConnect)
      say(maxConnect)

      nodes(index) = node.copy(
        c0 = node.c0.constrain(minConnect, maxConnect),
        c1 = node.c1.constrain(minConnect, maxConnect)
      )
    }

    
    val lastIC    = inputNodes + ((lookBack - 1) * layerSize)
    val lastLayer = inputNodes + (layers*layerSize)

    index match {
      case x if x < lastIC => repairInputConnected
      case x if x < lastLayer => repairInternal
      case _ => repairOutput
    }
  }
}
