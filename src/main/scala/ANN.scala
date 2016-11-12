package GA

object ANN {

    type SVector[A] = Vector[A]
    import breeze.linalg._
    import breeze.numerics._

    def createMatrix(ws: List[List[Double]]): DenseMatrix[Double] = 
        DenseMatrix(ws.map(_.toArray):_*)

    //%import scalaz.State
    //%import scalaz.State._
    import scalaz._
    import Scalaz._

    def consume[A](a: Int) = State[Seq[A], Seq[A]] { λ => {
        val (α, β) = λ.splitAt(a) 
        (β, α)
    }}
    
    def consumeList[A](points: Seq[Int]): State[ Seq[A], Seq[Seq[A]] ] = for {

        slices ←  points.foldLeft(State[ Seq[A], Seq[Seq[A]]](λ => (λ, Nil)))( (s, index) => for {
            list ←  s
            slice ←  consume(index)
        } yield (slice +: list))

    } yield (slices.reverse)


    case class FeedForward(
        weights: List[(DenseMatrix[Double], DenseMatrix[Double])],
        activator: Double => Double)
    {

        def runV(input: List[Double]): DenseMatrix[Double] = {
            ( DenseMatrix(input.toArray) /: weights)( (inputs, weights) => {

                val a = inputs * weights._1
                val b = a + weights._2

                // println("------------")
                // println("inputs")
                // println(inputs)

                // println("------------")
                // println("weights._1 (in)")
                // println(weights._1)

                // println("------------")
                // println("weights._2 (bias)")
                // println(weights._2)

                // println("------------")
                // println("a")
                // println(a)

                // println("------------")
                // println("b")
                // println(b)

                // println("############\n\n")

                b.map(activator)
            })
        }
        def run(input: List[Double]): List[Double] = runV(input).toArray.toList

    }
    object FeedForward {

        def fromGenome(
            weightRange: (Double, Double),
            activator: Double => Double,

            layers: List[Int],
            genome: Representations.MultiBitGenome

        ): FeedForward = {

            val layout = layers zip (layers.tail)
            val nodesCount = layers.sum
            val inputNodesCount = layers.head
            val weightsCount = layout.map( λ => λ._1 * λ._2 ).sum

            val geneExpressions = genome.genes.map(_.toDouble)
            val weightExpressions = (geneExpressions.map(λ => (λ*(weightRange._2 - weightRange._1)) + weightRange._1).toList)

            def makeLayerRow: State[ (List[(Int, Int)], List[Double]), (DenseMatrix[Double], DenseMatrix[Double]) ] = State( λ => {

                val layout = λ._1
                val weights = λ._2

                val layer = layout.head
                val (layerWeights, r) = weights.splitAt(layer._1 * layer._2)
                val (biasWeights, rest) = r.splitAt(layer._2)



                val rowLen = layer._2
                val rowNum = layer._1
                val weightMatrix = createMatrix(layerWeights.grouped(rowLen).toList)
                val biasMatrix = DenseMatrix(biasWeights.toArray)

                // println("making row")
                // println("----------")
                // println("layout")
                // println(layout)
                // println("----------")
                // println("weights")
                // println(weights)
                // println("----------")
                // println("layer")
                // println(layer)
                // println("----------")
                // println("rowLen")
                // println(rowLen)
                // println("----------")
                // println("rowNum")
                // println(rowNum)
                // println("----------")
                // println("layerWeights")
                // println(layerWeights)
                // println("----------")
                // println("biasWeights")
                // println(biasWeights)
                // println("----------")
                // println("layer._1 * layer._2")
                // println(layer._1 * layer._2)
                // println("----------")

                // println("##########")
                // println("##########")
                // println("##########")

                // println("Created weightMatrix: ")
                // println(weightMatrix)

                // println("Created biases: ")
                // println(biasMatrix)


                ( (layout.tail, rest), (weightMatrix, biasMatrix) )
            })

            type a[A] = State[ (List[(Int, Int)], List[Double]), A]
            val weights = ((List.fill(layout.length)(makeLayerRow)).sequence[a, (DenseMatrix[Double], DenseMatrix[Double]) ]).eval( (layout, weightExpressions) )

            FeedForward(
                weights,
                activator
            )
        }
    }

            


    case class CTRNN(
        weights: DenseMatrix[Double],
        nodes: DenseVector[Double],     // 1D
        gain: DenseVector[Double],      // 1D
        τ: DenseVector[Double],

        inputCount: Int,
        neuronCount: Int
    ) {

        override def toString: String = 
            nodes.toString


        def run(input: List[Double]): (CTRNN, List[Double]) = {

            val in = DenseVector( (1.0 :: input).toArray )  // prepend bias node as input
            val nextO = DenseVector.vertcat(in, calculateOutput(nodes))
            val nextS = (nextO.asDenseMatrix * weights)
            val nextY = updateNodes(nextS(0, ::).t)
            (copy(nodes = nextY), nextY.toArray.toList)
        }

        def calculateOutput(nodes: DenseVector[Double]): DenseVector[Double] =
            (nodes :* gain).map(λ => (1.0/(1.0 + Math.exp(λ))))
        

        def updateNodes(signals: DenseVector[Double]): DenseVector[Double] = {
            val leak = - nodes
            val nodeInput = signals.slice(inputCount + 1, inputCount + neuronCount + 1)
            (leak + nodeInput) :/ τ 
        }
        
    }
    object CTRNN {

        def fromGenome(
            weightRange: (Double, Double),
            biasRange: (Double, Double),
            tauRange: (Double, Double),
            gainRange: (Double, Double),

            layers: List[Int],

            genome: Representations.MultiBitGenome): CTRNN = 
        {

            // From the list describing the layers decide how the internal structure of the 
            // CTRNN should look like
            val inputNodesCount = layers.head
            val internalNodesCount = layers.tail.sum
            val topLayerCount = layers(1)
            val nodesCount = inputNodesCount + internalNodesCount + 1

            val layout = layers zip (layers.tail :+ 0)

            val biasWeightCount = internalNodesCount
            val inputWeigthCount = layout.head match { case (l, r) => l*r }
            val internalWeightCount = layout.tail.map( { case(l, r) => l*l + l*r } ).sum

            val tauCount = internalNodesCount
            val gainCount = internalNodesCount

            val geneExpressions = genome.genes.map(_.toDouble)

            // Extract the various genes from the genome, create a list of list of genes
            // where each sublist corresponds to some parameter, like the internal node gains.

            val expressionSlices = consumeList[Double](List(
                    biasWeightCount,
                    inputWeigthCount,
                    internalWeightCount,
                    tauCount,
                    gainCount))(geneExpressions)._2
            

            // Calculate the final genetic expressions
            def toRange( range: (Double, Double), gene: Double): Double =
                (gene*(range._2 - range._1)) + range._1

            val biasExpression = expressionSlices(0).map(toRange(biasRange, _))
            val inputExpression = expressionSlices(1).map(toRange(weightRange, _))
            val internalExpression = expressionSlices(2).map(toRange(weightRange, _))
            val tauExpression = expressionSlices(3).map(toRange(tauRange, _))
            val gainExpression = expressionSlices(4).map(toRange(gainRange, _))


            // Create the final ANN vectors and matrices
            val biasWeights = (List.fill(1 + inputNodesCount)(.0) ++ biasExpression)
            val inputsWeights = {
                val rows = inputExpression.grouped(topLayerCount).toList
                rows.map(
                    List.fill(1 + inputNodesCount)(.0) ++ _ ++ 
                    List.fill(internalNodesCount - topLayerCount)(.0))
            }
            
            // Creates rows for the internal weights
            val internalWeights = {

                // For a layer, create the layer specific rows. 
                // Takes in the list of remaining weights and an indent, returns the remainder
                // of the list and the new indent
                def makeLayerRows(
                    layerFeedback: Int, 
                    layerOutputs: Int) = State[(Seq[Double], Int), Seq[Seq[Double]]] { λ => 
                        {
                            val rowLen = layerFeedback + layerOutputs
                            val rowNum = layerFeedback
                            val indent = λ._2
                            val remaining = nodesCount - (indent + rowLen)
                            val nextIndent = indent + layerFeedback

                            val (weights, rest) = λ._1.splitAt( rowLen * rowNum )

                            val row = weights.grouped(rowLen).toList.map(
                                List.fill(indent)(.0) ++ _ ++
                                List.fill(remaining)(.0))
                            ((rest, (indent + layerFeedback)), row)
                        }
                }
                
                // creates a state action chaining together calls for each layer
                def makeRows: State[ (Seq[Double], Int), Seq[Seq[Double]] ] = for {
                    layerRows ←  layout.tail.foldLeft( State[(Seq[Double], Int), Seq[Seq[Double]]]( λ => (λ, Nil)))( (s, layer) => for {
                        rows ←  s
                        next ←  makeLayerRows(layer._1, layer._2)
                    } yield (rows ++ next))
                } yield (layerRows)
                makeRows((internalExpression, inputNodesCount + 1))._2.map(_.toList).toList
            }

            val weights: List[List[Double]] = List(biasWeights) ::: inputsWeights ::: internalWeights
            val weightsMatrix = DenseMatrix(weights.map(_.toArray):_*)
            val gainsVector = DenseVector(gainExpression.toArray)
            val tauVector = DenseVector(tauExpression.toArray)
            val nodes = DenseVector.zeros[Double](internalNodesCount)

            CTRNN(
                weightsMatrix,
                nodes,
                gainsVector,
                tauVector,
                inputNodesCount,
                internalNodesCount
            )

        }
    }
}
