package GA

import scala.util.Random

object tracker {

    val gameLength = 30
    val gameHeight = 15
    val agentLength = 5

    case class Beer(x: Int, y: Int, s: Int)
    case class Agent(x: Int)
    case class Feedback(telemetry: List[Int], event: Option[Event])
    case class Move(dir: Int, pull: Boolean)

    trait Event
    case object Miss extends Event
    case object Hit extends Event
    case object Avoidance extends Event
    case class PartialHit(hit: List[Int]) extends Event
    case class Splat(hit: List[Int]) extends Event

    def translate(x: Int): Int = if (x >= gameLength) x - gameLength else (if (x < 0) x + gameLength else x)

    // Return a list of intersections
    def getFeedback(beer: Beer, agent: Agent): Feedback = {
        val b = beer.copy(x = beer.x - agent.x)
        val intersects = 
            (b.x >= 0 && b.x <= agentLength) || ( (b.x + b.s) >= 0 && (b.x + b.s) <= agentLength)

        val telemetry = {
            if (intersects) {
                val leftMost =  if (b.x < 0) 0 else b.x
                val rightMost = if (b.x + b.s > agentLength) agentLength else b.x + b.s
                List.fill(leftMost)(0) ::: List.fill(rightMost - leftMost)(1) ::: List.fill( agentLength - rightMost)(0)
            }
            else List.fill(agentLength)(0)
        }

        val event =
            if (b.y < 1)
                if (intersects) 
                    if (telemetry.sum >= b.s) Some(Hit)
                    else { 
                        if(b.s > 4) Some(Splat(telemetry))
                        else Some(PartialHit(telemetry)) }
                else 
                    if (b.s > 4) Some(Avoidance) else Some(Miss)
            else None
        
        Feedback(telemetry, event)
    }

    def updateAgentToroidal(a: Agent, m: Int): Agent = {
        a.copy(x = translate(a.x + m))
    }

    def updateAgentBordered(a: Agent, m: Int): Agent = {
        val tpos = a.x + m
        val pos = if (tpos < 0) 0 else (if (tpos + agentLength > 29) 29 else tpos)
        Agent(pos)
    }

    case class Game(
        agent: Agent, beer: Beer,
        updateAgent: (Agent, Int) => Agent,
        updateBeer: Game => Game
    ) {

        def performAction(move: Move): (Game, Feedback) = {

            val nextAgent = updateAgent(agent, move.dir)
            val nextBeer  = beer.copy(y = if(move.pull) 0 else beer.y - 1)

            val feedback = getFeedback(nextBeer, nextAgent)
            
            if (nextBeer.y > 0)
                (copy(agent = nextAgent, beer = nextBeer), feedback)
            else
                (updateBeer(this), feedback)
        }
    }
    object Game {
        def newGame(updateAgent: (Agent, Int) => Agent, updateBeer: Game => Game): (Feedback, Game) = {
            val agent = Agent(Random.nextInt(30))
            val beer = Beer(3 + Random.nextInt(20), 15, 2)

            (getFeedback(beer, agent), Game(agent, beer, updateAgent, updateBeer))
        }
    }


    import scalaz.State
    import scalaz.State._

    import ANN.CTRNN

    def interpretOutput(output: List[Double]): Move = {

        val l = output.length

        val leftPull = math.round((1.5*output(l - 1)).toFloat).toInt
        val rightPull = math.round((1.5*output(l - 2)).toFloat).toInt

        def toRange(bot: Int, top: Int, x: Int): Int = 
            if (x < bot) bot else( if(x > top) top else x)

        val dir = toRange(-4, 4, (rightPull - leftPull))
        val pullDown = false

        Move(dir, pullDown)
    }

    def runMove(m: Move): State[Game, Feedback] = State[Game, Feedback] {
        game => game.performAction(m) }

    def runNet(in: List[Int]): State[CTRNN, List[Double]] = State[CTRNN, List[Double]] {
        net => net.run(in.map(_.toDouble)) }
    
    def playMove(in: List[Int]): State[(CTRNN, Game), (Game, Feedback)] = State[(CTRNN, Game), (Game, Feedback)] {
        stateTuple => {

            val net = stateTuple._1
            val game = stateTuple._2
        
            val (nextNet, nextMove) = runNet(in)(net)
            val move = interpretOutput(nextMove)
            val (nextGame, feedback) = runMove(move)(game)

            ((nextNet, nextGame), (game, feedback))
        }
    }

    def playGame(moves: Int, init: Feedback): State[(CTRNN, Game), List[(Game, Feedback)]] = for {
        fb ←  (0 until moves).foldLeft( State[ (CTRNN, Game), List[(Game, Feedback)]](λ => (λ, List((λ._2, init)))))( (s, index) => for {
            in ←  s
            next ←  playMove(in.head._2.telemetry)
        } yield (next :: in))
    } yield (fb)


    def evaluate(
        translate: Representations.MultiBitGenome => CTRNN, 
        genome: Representations.MultiBitGenome,
        updateAgent: (Agent, Int) => Agent
    ): Double = {

        val babby = trackScore.babbyCup(translate, genome, updateAgent)
        if (babby < 3.0) babby
        else {
            val shitter = trackScore.shitterCup(translate, genome, updateAgent)
            if (shitter < 3.0) shitter + 3.0
            else {
                val gitgud = trackScore.darkSouls(translate, genome, updateAgent)
                if (gitgud < 1.0) gitgud + 2.0
                else {
                    val strelok = trackScore.STALKER(translate, genome, updateAgent)
                    if (strelok < 1.0) strelok + 3.0
                    else {
                        val flash = trackScore.broodWar(translate, genome, updateAgent)
                        if (flash < 1.0) flash + 4.0
                        else 5.0
                    }
                }
            }
        }
    }

    def evaluateHistory(
        translate: Representations.MultiBitGenome => CTRNN, 
        genome: Representations.MultiBitGenome,
        updateAgent: (Agent, Int) => Agent
    
    ): (List[Game], List[Feedback]) = {

        trackScore.Standard.standard(translate, genome, updateAgent)
    }
}
