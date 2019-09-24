package scalapagos

import IntBonusOps._

trait Modifiers {
  def size: Int
  def nodes: Array[Node]
  def clamp(n: Int): Int = if(n < 0) 0 else if(n >= size) size - 1 else n

  def modify(node: Node, idx: Int): (Array[Node], Boolean) = node.f match {

    /** Add P1 new random nodes after (P0 + idx). */
    case x @ ADD => {
      val nodesToAdd       = node.p1.toInt
      val insertionAddress = clamp(idx + node.p0.toInt)

      val randomNodes = Array.fill(node.p1.toInt)(Node.randomNode)

      val next = nodes.slice(0, insertionAddress) ++ randomNodes ++ nodes.slice(insertionAddress, size)

      (next, true)
    }


    /** Delete the nodes between (P0 + idx) and (P0 + idx + P1). */
    case x @ DEL => {
      val delFrom = clamp(idx + node.p0.toInt)
      val delTo   = clamp(idx + node.p0.toInt + node.p1.toInt)

      if(delFrom < delTo){
        (nodes.slice(0, delFrom) ++ nodes.slice(delTo, size),
          true)
      }
      else
        (nodes, false)
    }


    /** Move the nodes between (P0 + x) and (P0 + x + P1) and insert after (P0 + x + P2). */
    case x @ MOV => {
      val readFrom    = clamp(node.p0.toInt + idx)
      val readTo      = clamp(node.p0.toInt + idx + node.p1.toInt)
      val destination = clamp(node.p0.toInt + node.p2.toInt + idx)

      if((readFrom < readTo) && !(readFrom to readTo contains destination)){
        val splice = nodes.slice(readFrom, readTo)
        if(readFrom > destination){
          (true, nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, readFrom) ++ nodes.slice(readTo, size))
        }
        else{
          (true, nodes.slice(0, readFrom) ++ nodes.slice(readTo, destination) ++ splice ++ nodes.slice(destination, size))
        }
      }
      else{
        (nodes, false)
      }
    }


    /**
      Copy the nodes between (P0 + idx) and (P0 + idx + P1) to position (P0 + x + P2),
      replacing existing nodes in the target position.
      */
    case x @ OVR => {
      val readFrom = clamp(node.p0.toInt + idx)
      val readTo   = clamp(node.p0.toInt + idx + node.p1.toInt)
      val writeTo  = clamp(node.p0.toInt + idx + node.p2.toInt)
      val len      = readFrom - readTo

      if(readFrom < readTo){
        val splice = nodes.slice(readFrom, readTo)
        (nodes.slice(0, writeTo) ++ splice ++ nodes.slice(writeTo + len, size), true)
      }
      else
        (nodes, false)
    }


    /**
      Copy the nodes between (P0) and (P0 + P1) and insert after (P0 + P2).
      */
    case x @ DU2 => {
      val dupFrom     = clamp(node.p0.toInt)
      val dupTo       = clamp(node.p0.toInt + node.p1.toInt)
      val destination = clamp(node.p0.toInt + node.p2.toInt)

      if(dupFrom < dupTo){
        val splice = nodes.slice(dupFrom, dupTo)
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
      val dupFrom       = clamp(node.p0.toInt)
      val dupTo         = clamp(node.p0.toInt + node.p1.toInt)
      val destination   = clamp(node.p0.toInt + node.p2.toInt)
      val shiftDistance = destination - dupFrom

      if((dupFrom < dupTo) && (shiftDistance > 0)){
        val splice = nodes.slice(dupFrom, dupTo)
        for(ii <- 0 until splice.size){
          splice(ii) = splice(ii).copy(
            c0 = splice(ii).c0 + shiftDistance,
            c1 = splice(ii).c1 + shiftDistance
          )
        }
        (nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, size), true)pv
      }
      else{
        (nodes, false)
      }
    }


    /**
      Starting from position (P0 + idx) copy (P1) nodes and insert after the node
      at position (P0 + idx + P1). During the copy, the c_ij of copied nodes are multiplied by P2.
      */
    case x @ DU4 => {
      val dupFrom       = clamp(node.p0.toInt + idx)
      val dupTo         = clamp(dupFrom + node.p1.toInt)
      val destination   = clamp(node.p0.toInt + node.p1.toInt + idx)

      if(dupFrom < dupTo){
        val splice = nodes.slice(dupFrom, dupTo)
        for(ii <- 0 until splice.size){
          splice(ii) = splice(ii).copy(
            c0 = (splice(ii).c0.toDouble * node.p2).toInt,
            c1 = (splice(ii).c1.toDouble * node.p2).toInt
          )
        }
        nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, size)
      }
      else{
        nodes
      }
    }


    /**
      Not really an SM node, just a marker
      */
    case x @ COPYSTOP => nodes


    /**Copy from x to the next COPYTOSTOP or STOP function node, or the end of the graph.
      Nodes are inserted at the position the operator stops at
      */
    case x @ COPYTOSTOP => {
      val copyStop = {
        val cs = nodes.indexWhere(n => n.f == COPYSTOP, idx)
        if(cs == -1) size else cs
      }

      val copySlice = nodes.slice(idx, copyStop)
      nodes.slice(0, copyStop) ++ copySlice ++ nodes.slice(copyStop, size)
    }


    /**
      Starting at node index (P0 + idx), add P2 to the values of the c_ij of next P1 nodes.
      */
    case x @ SHIFTCONN => {
      val shiftFrom = clamp(node.p0.toInt + idx)
      val shiftTo   = clamp(shiftFrom + node.p1.toInt)
      for(ii <- shiftFrom until shiftTo){
        nodes(ii) = nodes(ii).copy(
          c0 = nodes(ii).c0 + node.p2.toInt,
          c1 = nodes(ii).c1 + node.p2.toInt
        )
      }
      nodes
    }


    case x @ SHIFTCONN2 => {
      val shiftFrom = clamp(node.p0.toInt + idx)
      val shiftTo   = clamp(shiftFrom + node.p1.toInt)
      for(ii <- shiftFrom until shiftTo){
        nodes(ii) = nodes(ii).copy(
          c0 = (nodes(ii).c0.toDouble + node.p2).toInt,
          c1 = (nodes(ii).c1.toDouble + node.p2).toInt
        )
      }
      nodes
    }


    // This seems wrong, there are only two connections...
    /**
      Change the (P1 mod 3)th connection of node P0 to P2.
      We're gonna go with mod 2
      */
    case x @ CHC => {
      val changeIdx = clamp(node.p0.toInt)
      val newConnection = node.p2.toInt
      if(newConnection > 0){
        val connectionToChange = node.p1.toInt.modp(2)
        if(connectionToChange == 0)
          nodes(changeIdx) = nodes(changeIdx).copy(c0 = newConnection)
        else
          nodes(changeIdx) = nodes(changeIdx).copy(c0 = newConnection)
      }
      (nodes, false)
    }


    /**
      Change the function of node P0 to the function associated with P1
      */
    case x @ CHF => {
      val changeIdx = clamp(node.p0.toInt)
      nodes(changeIdx) = nodes(changeIdx).copy(f = Node.lookupTable(node.p1.toInt.modp(Node.lookupTable.size)))
      (nodes, true)
    }


    /**
      Change the (P1 mod 3)th argument of node P0 to P2.
      */
    case x @ CHP => {
      val changeIdx = clamp(node.p0.toInt)
      val connectionToChange = node.p1.toInt.modp(3)
      connectionToChange match {
        case 0 => nodes(changeIdx).copy(p0 = node.p2)
        case 1 => nodes(changeIdx).copy(p0 = node.p2)
        case 2 => nodes(changeIdx).copy(p0 = node.p2)
        case _ => nodes(changeIdx).copy(p0 = node.p2)
      }
      (nodes, true)
    }
  }
  ???
}
