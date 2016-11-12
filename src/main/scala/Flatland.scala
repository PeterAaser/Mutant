package GA

import scala.util.Random

// Flatland is an NxN grid environment, featuring food, poison and empty squares
// It is toroidal

object FlatLand {

    case class Direction(x: Int, y: Int){
        def left: Direction = Direction(-y, x)
        def right: Direction = Direction(y, x)
    }

    case class Position(x: Int, y: Int)

    sealed trait Tile
    case object AIDS extends Tile  { override def toString: String = "X" }
    case object Gainz extends Tile { override def toString: String = "O" }
    case object Empty extends Tile { override def toString: String = "_" }

    case class Dude(pos: Position, dir: Direction) extends Tile  { 

        override def toString: String = dir match {
            case Direction(0, -1)  => "↑"
            case Direction(-1, 0) => "←"
            case Direction(0, 1) => "↓"
            case Direction(1, 0)  => "→"
            case _ => "?"
        }

        val leftX  = pos.x + dir.left.x
        val midX   = pos.x + dir.x
        val rightX = pos.x + dir.right.x

        val leftY  = pos.y + dir.left.y
        val midY   = pos.y + dir.y
        val rightY = pos.y + dir.right.y

        def poke = {
            println("[" + pos.x + "][" + pos.y + "]" + ", " + this)
            println("left: [" + leftX + "][" + leftY + "]")
            println("front: [" + midX + "][" + midY + "]")
            println("right: [" + rightX + "][" + rightY + "]")
        }
    }

    case class FlatRunner(board: List[List[Tile]], agent: Dude) {

        def noop[S] = ST[S, Unit](())

        def agentVision[S](board: STArray2[S,Tile], agent: Dude): ST[S, List[Tile]] = for {
            left   ←  FlatRunner.getTile(board, agent.leftX, agent.leftY)
            middle ←  FlatRunner.getTile(board, agent.midX, agent.midY)
            right  ←  FlatRunner.getTile(board, agent.rightX, agent.rightY)

            _ = println()
            _ = println(agent.dir.x)
            _ = println(agent.dir.y)

        } yield (left :: middle :: right :: Nil)

        def getFeedback(ts: List[Tile]): (List[Double]) = {
            val p = ts.map(λ => λ match { case AIDS => 1.0; case _ => .0})
            val g = ts.map(λ => λ match { case Gainz => 1.0; case _ => .0})
            p ::: g
        }

        def interpretFeedback(in: List[Double], d: Direction): Direction = {
            if(in(0) > in(1) && in(0) > in(2)) d.left 
            else
                if(in(2) > in(1)) d.right else d 
        }

        def doShit(brain: ANN.FeedForward): Unit = {

            ST.runST(new RunnableST[Unit] {
                def apply[S] = for {

                    b ←  STArray2.fromList(board)

                    xPos ←  STRef(agent.pos.x)
                    yPos ←  STRef(agent.pos.y)

                    xDir ←  STRef(agent.dir.x)
                    yDir ←  STRef(agent.dir.y)

                    tiles ←  (0 until 20).foldLeft(noop[S])( (s, _) => for {

                        _ ←  s

                        xp ←  xPos.read
                        yp ←  yPos.read

                        xd ←  xDir.read
                        yd ←  yDir.read

                        vision ←  agentVision(b, Dude(Position(xp, yp), Direction(xd, yd)))

                        feedback = getFeedback(vision)
                        reaction = brain.run(feedback)
                        decision = interpretFeedback(reaction, Direction(xd, yd))
                        tile ←  FlatRunner.performAction(b, Dude(Position(xp, yp), Direction(xd, yd)), decision)

                        _ ←  xPos.write( if(xp + decision.x < 0){ xp + decision.x + 10 } else { if(xp + decision.x > 9){ xp + decision.x - 10 } else { xp + decision.x }} )
                        _ ←  yPos.write( if(yp + decision.y < 0){ yp + decision.y + 10 } else { if(yp + decision.y > 9){ yp + decision.y - 10 } else { yp + decision.y }} )

                        f ←  b.freeze
                        _ = println(f.map(_.mkString(" ")).mkString("\n"))

                    } yield ())
                } yield ()
            })
        }



        def doSomeCoolShit: Tile = {

            ST.runST(new RunnableST[Tile] {
                def apply[S] = for {
                    b ←  STArray2.fromList(board)
                    t ←  FlatRunner.performAction(b, agent, Direction(1, 0))
                    // seent ←  agentVision(b, agent)
                } yield (t)
            })
        }

        override def toString: String =
            board.map(_.mkString(" ")).mkString("\n")
    }
    object FlatRunner {

        def translate(x: Int, y: Int, xDim: Int, yDim: Int): (Int, Int) = {
            val xi = if(x < 0) x + xDim else (if(x >= xDim) x % xDim else x)
            val yi = if(y < 0) y + yDim else (if(y >= yDim) y % yDim else y)
            (xi, yi)
        }

        def noop[S] = ST[S, Unit](())

        def getTile[S](board: STArray2[S,Tile], x: Int, y: Int): ST[S,Tile] = for {

            xMax ←  board.xSize
            yMax ←  board.ySize
            t    ←  board.read( translate(x, y, xMax, yMax)._2, translate(x, y, xMax, yMax)._1 )

        } yield (t)

        def performAction[S](board: STArray2[S,Tile], agent: Dude, dir: Direction): ST[S,Tile] = {
            val ymove = agent.pos.x + dir.x
            val xmove = agent.pos.y + dir.y

            val newpos = translate(xmove, ymove, 10, 10)

            for {
                nextTile ←  getTile(board, xmove, ymove)
                _ ←  board.write(agent.pos.x, agent.pos.y, Empty)
                t ←  getTile(board, xmove, ymove)
                _ ←  board.write(newpos._1, newpos._2, agent)
            } yield (t)
        }

        def apply(xDim: Int, yDim: Int): FlatRunner = {
            val (board, agent) = makeBoard(xDim, yDim)
            FlatRunner(board, agent)
        }
    }


    def makeBoard(xDim: Int, yDim: Int): (List[List[Tile]], Dude) = {

        val food = (0.33 * xDim * yDim).toInt
        val poison = (0.22 * xDim * yDim).toInt
        def noop[S] = ST[S, Unit](())

        val (agentX, agentY) = (Random.nextInt(xDim), Random.nextInt(yDim))

        def place[S](board: STArray2[S,Tile], t: Tile): ST[S, (Int, Int)] = { 
            
            for {
                _ ←  noop[S]
                x =  Random.nextInt(xDim)
                y =  Random.nextInt(yDim)

                tile ←  (board.read(x, y))

                λ ←  if (tile == Empty) (for {
                    _ ←  board.write(x, y, t)
                } yield ( (x, y) ))

                else place(board, t)

            } yield ( (λ._1, λ._2) )
        }

        def populate[S](board: STArray2[S,Tile]): ST[S,Dude] = for {

            _ ←  (0 until food).foldLeft(noop[S])( (s, _) => for {
                _ ←  s
                _ ←  place(board, Gainz)
            } yield ())

            _ ←  (0 until poison).foldLeft(noop[S])( (s, _) => for {
                _ ←  s
                _ ←  place(board, AIDS)
            } yield ())

            agent ←  place(board, Dude(Position(0, 0), Direction(0, 1)))
        } yield (Dude(Position(agent._2, agent._1), Direction(0, 1)))


        ST.runST(new RunnableST[(List[List[Tile]], Dude)] {
            def apply[S] = for {
                board ←  STArray2[S, Tile](xDim, yDim, Empty)
                agent ←  populate(board)
                flat ←  board.freeze
                _ = println("runnan")
            } yield ( (flat, agent) )
        })
    }
}
