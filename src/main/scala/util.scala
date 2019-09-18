package scalapagos

import sourcecode._

case class Dims(x: Int, y: Int){
  val total = x*y
}

object utils {
  def say(word: Any, color: String = Console.RESET, timestamp: Boolean = false)(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    cats.effect.IO
    val fname = filename.value.split("/").last
    println(Console.YELLOW + s"[${fname}: ${sourcecode.Line()}]" + color + s" $word" + Console.RESET)
  }

  def saylb(word: Any, color: String = Console.RESET, timestamp: Boolean = false)(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    cats.effect.IO
    val fname = filename.value.split("/").last
    println(Console.YELLOW + s"[${fname}: ${sourcecode.Line()}]" + color + s"\n$word" + Console.RESET)
  }

  def expDecayStep(factor: Float, base: Float, x: Float): Float =
    base + (x - base)*math.exp(-factor).toFloat
}
