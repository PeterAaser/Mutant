package GA

object trackScore {
    
    import tracker._
    import scala.util.Random
    import ANN.CTRNN

    trait Rank
    case object Shitter  extends Rank
    case object Master   extends Rank

    case class Tally(
        hits: Int,
        misses: Int,
        avoids: Int,
        partialHits: List[List[Int]],
        partialAvoids: List[List[Int]]
    ){
        lazy val hitRatio = hits.toDouble/(hits + partialHits.length + misses).toDouble
        lazy val hitPartials = ((0.0 /: partialHits)( (acc, λ) => λ.sum + acc))

        lazy val avoidRatio = avoids/(partialAvoids.length)
        lazy val splatPartials = ((0.0 /: partialAvoids)( (acc, λ) => λ.sum + acc))
        lazy val splatPartialsRatio = splatPartials/( ((5 * partialAvoids.length) - splatPartials + avoids*5))
    }

    def tallyEvents(events: List[Option[Event]]): Tally = 
        ( Tally(0, 0, 0, List[List[Int]](), List[List[Int]]() ) /: events.flatten)( (tally, event) => event match {
            case Miss                  => tally.copy(misses = tally.misses + 1)
            case Hit                   => tally.copy(hits = tally.hits + 1)
            case Avoidance             => tally.copy(avoids = tally.avoids + 1)
            case PartialHit(λ)         => tally.copy(partialHits = λ :: tally.partialHits) 
            case Splat(λ)              => tally.copy(partialAvoids = λ :: tally.partialAvoids) 
        })
    

    def challenge(

        newBeer: Game => Game,
        scoreEvents: List[Option[Event]] => Double,

        translate: Representations.MultiBitGenome => CTRNN, 
        genome: Representations.MultiBitGenome,
        updateAgent: (Agent, Int) => Agent

    ): Double = {
        val net = translate(genome)
        val game = Game.newGame(updateAgent, newBeer)
        val testRunner = playGame(300, game._1)
        val testRun = testRunner( (net, game._2) )
        scoreEvents(testRun._2.map(_._2.event))
    }


    def babbyCup( translate: Representations.MultiBitGenome => CTRNN, genome: Representations.MultiBitGenome, updateAgent: (Agent, Int) => Agent): Double = {
        challenge( FarmVille.newBeer, FarmVille.scoreEvents, translate, genome, updateAgent) +
        challenge( WiiResort.newBeer, WiiResort.scoreEvents, translate, genome, updateAgent) +
        challenge( Uncharted.newBeer, Uncharted.scoreEvents, translate, genome, updateAgent)
    }
    
    def shitterCup( translate: Representations.MultiBitGenome => CTRNN, genome: Representations.MultiBitGenome, updateAgent: (Agent, Int) => Agent): Double = 
        challenge( STALKER.newBeer, FarmVille.scoreEvents, translate, genome, updateAgent) +
        challenge( BroodWar.newBeer, WiiResort.scoreEvents, translate, genome, updateAgent) +
        challenge( Quake.newBeer, Uncharted.scoreEvents, translate, genome, updateAgent)
            
    def darkSouls( translate: Representations.MultiBitGenome => CTRNN, genome: Representations.MultiBitGenome, updateAgent: (Agent, Int) => Agent): Double = 
        challenge( Uncharted.newBeer, Uncharted.scoreEvents, translate, genome, updateAgent)

    def STALKER( translate: Representations.MultiBitGenome => CTRNN, genome: Representations.MultiBitGenome, updateAgent: (Agent, Int) => Agent): Double = 
        challenge( STALKER.newBeer, STALKER.scoreEvents, translate, genome, updateAgent)

    def broodWar( translate: Representations.MultiBitGenome => CTRNN, genome: Representations.MultiBitGenome, updateAgent: (Agent, Int) => Agent): Double = 
        challenge( BroodWar.newBeer, BroodWar.scoreEvents, translate, genome, updateAgent)


    object FarmVille {
        def newBeer(g: Game): Game = {
            // val size = if(Random.nextDouble > 0.5) 10 else Random.nextInt(2) + 1
            val size = 7
            g.copy(beer = Beer(x = Random.nextInt(30), y = 15, size))
        }
        
        def scoreEvents(e: List[Option[Event]]): Double = {
            val t = tallyEvents(e)

            val score = (1.0 - t.splatPartialsRatio)
            if(score > 0.8) 1.0 else score
        }
    }

    object WiiResort {

        def newBeer(g: Game): Game = {
            // val size = Random.nextInt(3) + 1
            val size = 2
            g.copy(beer = Beer(x = Random.nextInt(30), y = 15, s = size))
        }

        def scoreEvents(e: List[Option[Event]]): Double = {
            val t = tallyEvents(e)

            val score = t.hitRatio
            if(score > 0.8) 1.0 else score
        }
    }

    object Uncharted { 

        def newBeer(g: Game): Game = {
            // val size = if(Random.nextDouble > 0.5) 7 else Random.nextInt(3) + 1
            val size = if(Random.nextDouble > 0.5) 7 else 2
            g.copy(beer = Beer(x = Random.nextInt(30), y = 15, s = size))
        }

        def scoreEvents(e: List[Option[Event]]): Double = {
            val t = tallyEvents(e)

            val score = ( (1.0 - t.splatPartialsRatio) + t.hitRatio)/2.0
            if(score > 0.7) 1.0 else score
        }
    }

    object Quake { 

        def newBeer(g: Game): Game = {
            val size = 7
            g.copy(beer = Beer(x = Random.nextInt(30), y = 15, s = size))
        }

        def scoreEvents(e: List[Option[Event]]): Double = {
            val t = tallyEvents(e)

            val score = (1.0 - t.splatPartialsRatio)
            if(score > 0.9) 1.0 else score
        }
    }

    object STALKER { 

        def newBeer(g: Game): Game = {
            val size = Random.nextInt(3) + 1
            g.copy(beer = Beer(x = Random.nextInt(30), y = 15, s = size))
        }

        def scoreEvents(e: List[Option[Event]]): Double = {
            val t = tallyEvents(e)

            val score = t.hitRatio
            if(score > 0.9) 1.0 else score
        }
    }

    object BroodWar { 

        def newBeer(g: Game): Game = {
            val size = if(Random.nextDouble > 0.5) 2 else 7
            g.copy(beer = Beer(x = Random.nextInt(30), y = 15, s = size))
        }

        def scoreEvents(e: List[Option[Event]]): Double = {
            val t = tallyEvents(e)

            val score = (t.hitRatio + (1.0 - t.splatPartialsRatio))/2.0
            if(score > 0.8) 1.0 else score
        }
    }

    object Standard { 

        def newBeer(g: Game): Game = {
            // val size = Random.nextInt(5) + 1
            val size = if(Random.nextDouble > 0.5) 7 else Random.nextInt(3) + 1

            g.copy(beer = Beer(x = Random.nextInt(30), y = 15, s = size))

            // g.copy(beer = Beer(x = Random.nextInt(30), y = 15, Random.nextInt(3) + 1))
        }

        def scoreEvents(e: List[Option[Event]]): Double = 0.0

        def standard(
            translate: Representations.MultiBitGenome => CTRNN, 
            genome: Representations.MultiBitGenome,
            updateAgent: (Agent, Int) => Agent
        ): (List[Game], List[Feedback]) = {

            val net = translate(genome)
            val game = Game.newGame(updateAgent, newBeer)
            val testRunner = playGame(400, game._1)
            val testRun = testRunner( (net, game._2) )
            (testRun._2.map(_._1), testRun._2.map(_._2))
        }
    }
}
