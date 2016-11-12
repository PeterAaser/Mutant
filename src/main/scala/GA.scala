package GA

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint._
import scalafx.scene.control._
import scalafx.scene.shape._
import scalafx.scene.input._
import scalafx.event.ActionEvent
import scalafx.animation.AnimationTimer

import scala._
import scalaz._
import Scalaz._

import Data._

import scala.reflect.macros.Context
import scala.reflect.runtime.universe._
import scala.language.experimental.macros

import scala.util.Random

import ANN._

import FlatLand._

import scala.io.Source
import org.sameersingh.scalaplot._
import gnuplot.GnuplotPlotter
import jfreegraph.JFGraphPlotter
import java.io._

object GAsolver extends JFXApp {

    import reflect.runtime.universe._

    implicit class ColonTypeExtender [T : TypeTag] (x : T) {
        def colonType = typeOf[T].toString
    }

    // ########################################
    // ########################################
    // ########################################
    // ########################################


    val testGenome = Representations.MultiBitGenome.init( 29, 4, (λ, µ) => (λ, µ), λ => λ )

    println(testGenome)

    val weightRange = (-1.0, 1.0)
    val layout = List(6, 2, 2, 3)

    val testNet = FeedForward.fromGenome(
        weightRange,
        λ => λ,
        layout,
        testGenome
    )

    val fug = testNet.run(List(.4, .3, .2, .5, .1, .6))
    println(fug)

    val game = FlatLand.makeBoard(10, 10)
    val runner = FlatLand.FlatRunner(game._1, game._2)

    println(runner)
    println(runner.doShit(testNet))

    // ########################################
    // ########################################
    // ########################################
    // ########################################

    var frame = 0

    stage = new JFXApp.PrimaryStage {


        title = "beers"
        scene = new Scene(300, 160) {

            onKeyPressed = (e: KeyEvent) => {
                frame = e.code match {
                    case KeyCode.A => frame + 1
                    case KeyCode.R => frame - 1
                    case _ => 0
                }
                frame = if(frame < 0) 0 else (if (frame > 599) 599 else frame)
                println(frame)
                content = Nil
            }
        }
    }
}
