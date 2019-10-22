package scalapagos

import IntBonusOps._
import DoubleBonusOps._
import utils._
import NodeLookup._

trait Modifiers {
  def size: Int
  var nodes: Array[Node]
  def clamp(n: Int): Int = if(n < 0) 0 else if(n >= size) size - 1 else n

  implicit var debugPrint = false
  def modify(idx: Int): Boolean = {

    val p0 = nodes(idx).p0
    val p1 = nodes(idx).p1
    val p2 = nodes(idx).p2

    val function = nodes(idx).f

    function match {

      /** Add P1 new random nodes after (P0 + idx). */
      // case nodes.ADD => {
      //   val nodesToAdd       = node.p1.floorInt
      //   val insertionAddress = clamp(idx + node.p0.floorInt)

      //   val randomNodes = Array.fill(node.p1.floorInt)(Node.randomNode)

      //   val next = nodes.slice(0, insertionAddress) ++ randomNodes ++ nodes.slice(insertionAddress, size)

      //   dsay(s"adding $nodesToAdd at address $insertionAddress")
      //   (next, true)
      // }


      /** Delete the nodes between (P0 + idx) and (P0 + idx + P1). */
      case DEL => {
        val delFrom = clamp(idx + p0.floorInt)
        val delTo   = clamp(idx + p0.floorInt + p1.floorInt)

        if(delFrom < delTo){
          dsay(s"deleting nodes $delFrom -- $delTo")
          nodes = nodes.slice(0, delFrom) ++ nodes.slice(delTo, size)
          true
        }
        else
          false
      }


      /** Move the nodes between (P0 + x) and (P0 + x + P1) and insert after (P0 + x + P2). */
      case MOV => {
        val readFrom    = clamp(p0.floorInt + idx)
        val readTo      = clamp(p0.floorInt + idx + p1.floorInt)
        val destination = clamp(p0.floorInt + p2.floorInt + idx)

        if((readFrom < readTo) && !(readFrom to readTo contains destination)){
          val splice = nodes.slice(readFrom, readTo)
          if(readFrom > destination){
            dsay(s"Moving nodes from [$readFrom, $readTo] to $destination")
            nodes = nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, readFrom) ++ nodes.slice(readTo, size)
            true
          }
          else{
            dsay(s"Moving nodes from [$readFrom, $readTo] to $destination")
            nodes = nodes.slice(0, readFrom) ++ nodes.slice(readTo, destination) ++ splice ++ nodes.slice(destination, size)
            true
          }
        }
        else{
          dsay(s"Mov failed moving [$readFrom, $readTo] to $destination")
          false
        }
      }


      /**
        Copy the nodes between (P0 + idx) and (P0 + idx + P1) to position (P0 + x + P2),
        replacing existing nodes in the target position.
        
        TODO: Should this be a failure if no change is made?
        */
      case OVR => {
        val readFrom = clamp(p0.floorInt + idx)
        val readTo   = clamp(p0.floorInt + idx + p1.floorInt)
        val writeTo  = clamp(p0.floorInt + idx + p2.floorInt)
        val len      = readTo - readFrom

        if(readFrom < readTo){
          val splice = nodes.slice(readFrom, readTo)
          dsay(s"overwriting nodes from [$readFrom, $readTo] to [$writeTo, ${writeTo + len}]")
          nodes = nodes.slice(0, writeTo) ++ splice ++ nodes.slice(writeTo + len, size)
          true
        }
        else {
          dsay(s"$function failed")
          false
        }
      }


      /**
        Copy the nodes between (P0 + idx) and (P0 + idx + P1) and insert after (P0 + idx + P2).
        */
      case DUP => {
        val readFrom    = clamp(p0.floorInt + idx)
        val readTo      = clamp(p0.floorInt + idx + p1.floorInt)
        val destination = clamp(p0.floorInt + idx + p2.floorInt)
        val len         = readTo - readFrom

        if(readFrom < readTo){
          val splice = nodes.slice(readFrom, readTo)
          dsay(s"duplicating nodes from [$readFrom, $readTo] into [$destination, ${destination + len}]")
          nodes = nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, size)
          true
        }
        else {
          dsay(s"$function failed")
          false
        }
      }


      /**
        Copy the nodes between (P0) and (P0 + P1) and insert after (P0 + P2).
        */
      case DU2 => {
        val readFrom    = clamp(p0.floorInt)
        val readTo      = clamp(p0.floorInt + p1.floorInt)
        val destination = clamp(p0.floorInt + p2.floorInt)
        val len         = readTo - readFrom

        if(readFrom < readTo){
          val splice = nodes.slice(readFrom, readTo)
          dsay(s"duplicating nodes from [$readFrom, $readTo] into [$destination, ${destination + len}]")
          nodes = nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, size)
          true
        }
        else{
          dsay(s"$function failed")
          false
        }
      }


      /**
        Copy the nodes between (P0 + idx) and (P0 + idx + P1) and insert after (P0 + idx + P2).
        When copying, this function modifies the c_ij of the copied nodes so that they continue
        to point to the original nodes.
        
        If the destination occurs before the copy area, the node is invalid to avoid cycles 
        */
      case DU3 => {
        val readFrom      = clamp(p0.floorInt)
        val readTo        = clamp(p0.floorInt + p1.floorInt)
        val destination   = clamp(p0.floorInt + p2.floorInt)
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
          nodes = nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, size)
          true
        }
        else{
          dsay(s"$function failed")
          false
        }
      }


      /**
        Starting from position (P0 + idx) copy (P1) nodes and insert after the node
        at position (P0 + idx + P1). During the copy, the c_ij of copied nodes are multiplied by P2.
        */
      case DU4 => {
        val readFrom      = clamp(p0.floorInt + idx)
        val readTo        = clamp(readFrom + p1.floorInt)
        val destination   = clamp(p0.floorInt + p1.floorInt + idx)
        val len           = readTo - readFrom

        if((readFrom < readTo) && p2 > 0){
          val splice = nodes.slice(readFrom, readTo)
          for(ii <- 0 until splice.size){
            dsay("before:")
            dsay(splice(ii))
            splice(ii) = splice(ii).copy(
              c0 = (splice(ii).c0.toDouble * p2).floorInt,
              c1 = (splice(ii).c1.toDouble * p2).floorInt
            )
            dsay("after:")
            dsay(splice(ii))
          }
          dsay(s"duplicating nodes from [$readFrom, $readTo] into [$destination, ${destination + len}], scaling connections")
          nodes = nodes.slice(0, destination) ++ splice ++ nodes.slice(destination, size)
          true
        }
        else{
          dsay(s"$function failed")
          false
        }
      }


      /**
        Not really an SM node, just a marker
        */
      case COPYSTOP => false


      /**Copy from x to the next COPYTOSTOP or STOP function node, or the end of the graph.
        Nodes are inserted at the position the operator stops at
        */
      case COPYTOSTOP => {
        val copyStop = {
          val cs = nodes.indexWhere(n => n.f == COPYSTOP, idx) + 1
          if(cs == -1) size else cs
        }

        val copySlice = nodes.slice(idx, copyStop)
        dsay(s"Duplicating nodes from [$idx, $copyStop] to ${copyStop + 1}")
        nodes = nodes.slice(0, copyStop) ++ copySlice ++ nodes.slice(copyStop, size)
        true
      }


      /**
        Starting at node index (P0 + idx), add P2 to the values of the c_ij of next P1 nodes.
        */
      case SHIFTCONN => {
        val shiftFrom = clamp(p0.floorInt + idx)
        val shiftTo   = clamp(shiftFrom + p1.floorInt)
        val shiftBy   = p2.floorInt

        if((shiftBy > 0) && (shiftFrom < shiftTo)){
          for(ii <- shiftFrom until shiftTo){
            nodes(ii) = nodes(ii).copy(
              c0 = nodes(ii).c0 + p2.floorInt,
              c1 = nodes(ii).c1 + p2.floorInt
            )
          }
          dsay(s"Shifted node connections at [$shiftFrom, $shiftTo] by $shiftBy")
          true
        }
        else{
          dsay(s"Shifted node connections at [$shiftFrom, $shiftTo] by $shiftBy failed")
          false
        }
      }


      /**
        Starting at node index (P0 + idx), multiply the c_ij of the next
        P1 nodes by P2 .
        */
      case SHIFTCONN2 => {
        val shiftFrom = clamp(p0.floorInt + idx)
        val shiftTo   = clamp(shiftFrom + p1.floorInt)
        val scaleBy   = p2

        if((p2 > 0.0) && (shiftFrom < shiftTo)){
          for(ii <- shiftFrom until shiftTo){
            nodes(ii) = nodes(ii).copy(
              c0 = (nodes(ii).c0.toDouble * scaleBy).floorInt,
              c1 = (nodes(ii).c1.toDouble * scaleBy).floorInt
            )
          }
          dsay(s"scaled node connections at [$shiftFrom, $shiftTo] by $scaleBy")
          true
        }
        else {
          dsay(s"$function failed")
          false
        }
      }


      // This seems wrong, there are only two connections...
      /**
        Change the (P1 mod 3)th connection of node P0 to P2.
        We're gonna go with mod 2
        */
      case CHC => {
        val changeIdx = clamp(p0.floorInt)
        val newConnection = p2.floorInt
        if(newConnection > 0){
          val connectionToChange = p1.floorInt.modp(2)
          if(connectionToChange == 0)
            nodes(changeIdx) = nodes(changeIdx).copy(c0 = newConnection)
          else
            nodes(changeIdx) = nodes(changeIdx).copy(c1 = newConnection)
        }
        (newConnection > 0)
      }


      /**
        Change the function of node P0 to the function associated with P1
        */
      case CHF => {
        val changeIdx = clamp(p0.floorInt)
        val nextFunction = p1.floorInt.modp(NodeLookup.names.size - 1)
        nodes(changeIdx) = nodes(changeIdx).copy(f = nextFunction)
        true
      }


      /**
        Change the (P1 mod 3)th argument of node P0 to P2.
        */
      case CHP => {
        val changeIdx = clamp(p0.floorInt)
        val connectionToChange = p1.floorInt.modp(3)
        connectionToChange match {
          case 0 => nodes(changeIdx) = nodes(changeIdx).copy(p0 = p2)
          case 1 => nodes(changeIdx) = nodes(changeIdx).copy(p1 = p2)
          case 2 => nodes(changeIdx) = nodes(changeIdx).copy(p2 = p2)
          case _ => nodes(changeIdx) = nodes(changeIdx).copy(p0 = p2)
        }
        true
      }

      case _ => false
    }
  }
}
