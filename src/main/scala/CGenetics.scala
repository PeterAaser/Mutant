package scalapagos

import utils._
import IntBonusOps._
import DoubleBonusOps._

import util.Random

object CGenetics {

  def generate(inputNodes: Int, outputNodes: Int, layers: Int, layerSize: Int, lookBack: Int) = {

    val nodes = Array.ofDim[CGNode](inputNodes + (layers*layerSize) + outputNodes)

    // Generate NOP input nodes
    for(ii <- 0 until inputNodes){
      say(s"input node: $ii")
      nodes(ii) = CGNode(0, 0, 0)
    }

    // Generate input connected nodes
    val inputConnected = Array.ofDim[Node](layerSize * lookBack)
    for(ii <- 0 until layerSize * lookBack){
      say(s"input connected node: ${ii + inputNodes}")
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

      say(s"internal node: $ii")
      say(s"current depth: $currentDepth")
      say(s"min connect: $minConnect")
      say(s"max connect: $maxConnect\n")

      val c0 = Random.between(minConnect, maxConnect + 1)
      val c1 = Random.between(minConnect, maxConnect + 1)

      nodes(ii) = CGNode(c0, c1, f)
    }


    // Generate output layer
    for(ii <- (inputNodes + (layers * layerSize)) until (inputNodes + (layers * layerSize) + outputNodes)){
      say(s"output node: $ii")
      val f = Random.nextInt(17)

      val currentDepth = ii%outputNodes

      val minConnect = 1 + currentDepth
      val maxConnect = lookBack*layerSize + currentDepth

      val c0 = Random.between(minConnect, maxConnect + 1)
      val c1 = Random.between(minConnect, maxConnect + 1)

      nodes(ii) = CGNode(c0, c1, f)
    }

    nodes
  }
}
