package scalapagos

import IntBonusOps._
import DoubleBonusOps._
import utils._

trait Modifiers {
  def size: Int
  def nodes: Array[Node]
  def clamp(n: Int): Int = if(n < 0) 0 else if(n >= size) size - 1 else n


  def modify(node: Node, idx: Int): (Array[Node], Boolean) = node.f match {

    /** Add P1 new random nodes after (P0 + idx). */
    case x @ ADD => {
      val nodesToAdd       = node.p1.floorInt
      val insertionAddress = clamp(idx + node.p0.floorInt)

      val randomNodes = Array.fill(node.p1.floorInt)(Node.randomNode)

      val next = nodes.slice(0, insertionAddress) ++ randomNodes ++ nodes.slice(insertionAddress, size)

      dsay(s"adding $nodesToAdd at address $insertionAddress")
      (next, true)
    }


    /** Delete the nodes between (P0 + idx) and (P0 + idx + P1). */
    case x @ DEL => {
      val delFrom = clamp(idx + node.p0.floorInt)
      val delTo   = clamp(idx + node.p0.floorInt + node.p1.floorInt)

      if(delFrom < delTo){
        dsay(s"deleting nodes $delFrom -- $delTo")
        (nodes.slice(0, delFrom) ++ nodes.slice(delTo, size), true)
      }
      else
        (nodes, false)
    }


    /** Move the nodes between (P0 + x) and (P0 + x + P1) and insert after (P0 + x + P2). */
    case x @ MOV => {
      val readFrom    = clamp(node.p0.floorInt + idx)
      val readTo      = clamp(node.p0.floorInt + idx + node.p1.floorInt)
      val destination = clamp(node.p0.floorInt + node.p2.floorInt + idx)

      if((readFrom < readTo) && !(readFrom to readTo contains destination)){
        val splice = nodes.slice(readFrom, readTo)
        if(readFrom > destination){
          dsay(s"Moving nodes from [$readFrom, $readTo] to $destination")
          (nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, readFrom) ++ nodes.slice(readTo, size), true)
        }
        else{
          dsay(s"Moving nodes from [$readFrom, $readTo] to $destination")
          (nodes.slice(0, readFrom) ++ nodes.slice(readTo, destination) ++ splice ++ nodes.slice(destination, size), true)
        }
      }
      else{
        dsay(s"Mov failed moving [$readFrom, $readTo] to $destination")
        (nodes, false)
      }
    }


    /**
      Copy the nodes between (P0 + idx) and (P0 + idx + P1) to position (P0 + x + P2),
      replacing existing nodes in the target position.
      
      TODO: Should this be a failure if no change is made?
      */
    case x @ OVR => {
      val readFrom = clamp(node.p0.floorInt + idx)
      val readTo   = clamp(node.p0.floorInt + idx + node.p1.floorInt)
      val writeTo  = clamp(node.p0.floorInt + idx + node.p2.floorInt)
      val len      = readTo - readFrom

      if(readFrom < readTo){
        val splice = nodes.slice(readFrom, readTo)
        dsay(s"overwriting nodes from [$readFrom, $readTo] to [$writeTo, ${writeTo + len}]")
        (nodes.slice(0, writeTo) ++ splice ++ nodes.slice(writeTo + len, size), true)
      }
      else {
        dsay(s"$x failed")
        (nodes, false)
      }
    }


    /**
      Copy the nodes between (P0 + idx) and (P0 + idx + P1) and insert after (P0 + idx + P2).
      */
    case x @ DUP => {
      val readFrom    = clamp(node.p0.floorInt + idx)
      val readTo      = clamp(node.p0.floorInt + idx + node.p1.floorInt)
      val destination = clamp(node.p0.floorInt + idx + node.p2.floorInt)
      val len         = readTo - readFrom

      if(readFrom < readTo){
        val splice = nodes.slice(readFrom, readTo)
        dsay(s"duplicating nodes from [$readFrom, $readTo] into [$destination, ${destination + len}]")
        (nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, size), true)
      }
      else {
        dsay(s"$x failed")
        (nodes, false)
      }
    }


    /**
      Copy the nodes between (P0) and (P0 + P1) and insert after (P0 + P2).
      */
    case x @ DU2 => {
      val readFrom    = clamp(node.p0.floorInt)
      val readTo      = clamp(node.p0.floorInt + node.p1.floorInt)
      val destination = clamp(node.p0.floorInt + node.p2.floorInt)
      val len         = readTo - readFrom

      if(readFrom < readTo){
        val splice = nodes.slice(readFrom, readTo)
        dsay(s"duplicating nodes from [$readFrom, $readTo] into [$destination, ${destination + len}]")
        (nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, size), true)
      }
      else{
        (nodes, false)
      }
    }


    /**
      Copy the nodes between (P0 + idx) and (P0 + idx + P1) and insert after (P0 + idx + P2).
      When copying, this function modifies the c_ij of the copied nodes so that they continue
      to point to the original nodes.
      
      If the destination occurs before the copy area, the node is invalid to avoid cycles 
      */
    case x @ DU3 => {
      val readFrom      = clamp(node.p0.floorInt)
      val readTo        = clamp(node.p0.floorInt + node.p1.floorInt)
      val destination   = clamp(node.p0.floorInt + node.p2.floorInt)
      val shiftDistance = destination - readFrom
      val len           = readTo - readFrom

      if((readFrom < readTo) && (shiftDistance > 0)){
        val splice = nodes.slice(readFrom, readTo)
        for(ii <- 0 until splice.size){
          splice(ii) = splice(ii).copy(
            c0 = splice(ii).c0 + shiftDistance,
            c1 = splice(ii).c1 + shiftDistance
          )
        }
        dsay(s"duplicating nodes from [$readFrom, $readTo] into [$destination, ${destination + len}], preserving connections")
        (nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, size), true)
      }
      else{
        dsay(s"$x failed")
        (nodes, false)
      }
    }


    /**
      Starting from position (P0 + idx) copy (P1) nodes and insert after the node
      at position (P0 + idx + P1). During the copy, the c_ij of copied nodes are multiplied by P2.
      */
    case x @ DU4 => {
      val readFrom      = clamp(node.p0.floorInt + idx)
      val readTo        = clamp(readFrom + node.p1.floorInt)
      val destination   = clamp(node.p0.floorInt + node.p1.floorInt + idx)
      val len           = readTo - readFrom

      if(readFrom < readTo){
        val splice = nodes.slice(readFrom, readTo)
        for(ii <- 0 until splice.size){
          splice(ii) = splice(ii).copy(
            c0 = (splice(ii).c0.toDouble * node.p2).floorInt,
            c1 = (splice(ii).c1.toDouble * node.p2).floorInt
          )
        }
        dsay(s"duplicating nodes from [$readFrom, $readTo] into [$destination, ${destination + len}], scaling connections")
        (nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, size), true)
      }
      else{
        (nodes, true)
      }
    }


    /**
      Not really an SM node, just a marker
      */
    case x @ COPYSTOP => (nodes, false)


    /**Copy from x to the next COPYTOSTOP or STOP function node, or the end of the graph.
      Nodes are inserted at the position the operator stops at
      */
    case x @ COPYTOSTOP => {
      val copyStop = {
        val cs = nodes.indexWhere(n => n.f == COPYSTOP, idx) + 1
        if(cs == -1) size else cs
      }

      val copySlice = nodes.slice(idx, copyStop)
      dsay(s"Duplicating nodes from [$idx, $copyStop] to ${copyStop + 1}")
      (nodes.slice(0, copyStop) ++ copySlice ++ nodes.slice(copyStop, size), true)
    }


    /**
      Starting at node index (P0 + idx), add P2 to the values of the c_ij of next P1 nodes.
      */
    case x @ SHIFTCONN => {
      val shiftFrom = clamp(node.p0.floorInt + idx)
      val shiftTo   = clamp(shiftFrom + node.p1.floorInt)
      val shiftBy   = node.p2.floorInt

      if((shiftBy > 0) && (shiftFrom < shiftTo)){
        for(ii <- shiftFrom until shiftTo){
          nodes(ii) = nodes(ii).copy(
            c0 = nodes(ii).c0 + node.p2.floorInt,
            c1 = nodes(ii).c1 + node.p2.floorInt
          )
        }
        dsay(s"Shifted node connections at [$shiftFrom, $shiftTo] by $shiftBy")
        (nodes, true)
      }
      else{
        dsay(s"Shifted node connections at [$shiftFrom, $shiftTo] by $shiftBy failed")
        (nodes, false)
      }
    }


    /**
      Starting at node index (P0 + idx), multiply the c_ij of the next
      P1 nodes by P2 .
      */
    case x @ SHIFTCONN2 => {
      val shiftFrom = clamp(node.p0.floorInt + idx)
      val shiftTo   = clamp(shiftFrom + node.p1.floorInt)
      val scaleBy   = node.p2

      if((node.p2 > 0.0) && (shiftFrom < shiftTo)){
        for(ii <- shiftFrom until shiftTo){
          nodes(ii) = nodes(ii).copy(
            c0 = (nodes(ii).c0.toDouble * scaleBy).floorInt,
            c1 = (nodes(ii).c1.toDouble * scaleBy).floorInt
          )
        }
        dsay(s"scaled node connections at [$shiftFrom, $shiftTo] by $scaleBy")
        (nodes, true)
      }
      else {
        dsay(s"$x failed")
        (nodes, false)
      }
    }


    // This seems wrong, there are only two connections...
    /**
      Change the (P1 mod 3)th connection of node P0 to P2.
      We're gonna go with mod 2
      */
    case x @ CHC => {
      val changeIdx = clamp(node.p0.floorInt)
      val newConnection = node.p2.floorInt
      if(newConnection > 0){
        val connectionToChange = node.p1.floorInt.modp(2)
        if(connectionToChange == 0)
          nodes(changeIdx) = nodes(changeIdx).copy(c0 = newConnection)
        else
          nodes(changeIdx) = nodes(changeIdx).copy(c1 = newConnection)
      }
      (nodes, (newConnection > 0))
    }


    /**
      Change the function of node P0 to the function associated with P1
      */
    case x @ CHF => {
      val changeIdx = clamp(node.p0.floorInt)
      nodes(changeIdx) = nodes(changeIdx).copy(f = Node.lookupTable(node.p1.floorInt.modp(Node.lookupTable.size - 1)))
      (nodes, true)
    }


    /**
      Change the (P1 mod 3)th argument of node P0 to P2.
      */
    case x @ CHP => {
      val changeIdx = clamp(node.p0.floorInt)
      val connectionToChange = node.p1.floorInt.modp(3)
      connectionToChange match {
        case 0 => nodes(changeIdx).copy(p0 = node.p2)
        case 1 => nodes(changeIdx).copy(p1 = node.p2)
        case 2 => nodes(changeIdx).copy(p2 = node.p2)
        case _ => nodes(changeIdx).copy(p0 = node.p2)
      }
      (nodes, true)
    }

    case _ => (nodes, false)
  }
}
