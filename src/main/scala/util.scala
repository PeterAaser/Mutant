package scalapagos

import sourcecode._

case class Dims(x: Int, y: Int){
  val total = x*y
}

object IntBonusOps {
  implicit class intBonusOps(i: Int){
    def modp(n: Int) = {
      val a = i % n
      if(a > 0) a else a + n
    }

    def min1 = if(i > 0) i else 1
    def downto (n: Int) = i to n by -1
    def downtil (n: Int) = i until n by -1

  }
}

object DoubleBonusOps {
  implicit class doubleBonusOps(d: Double){
    def floorInt: Int = d.floor.toInt
    def max1: Double = if(d > 1.0) 1.0 else d
    def sqrtOr0: Double = if(d < 0.0) 0.0 else math.sqrt(d)
  }
}

object utils {
  def say(word: Any, color: String = Console.RESET, timestamp: Boolean = false)(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    val fname = filename.value.split("/").last
    println(Console.YELLOW + s"[${fname}: ${sourcecode.Line()}]" + color + s" $word" + Console.RESET)
  }

  def saylb(word: Any, color: String = Console.RESET, timestamp: Boolean = false)(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    val fname = filename.value.split("/").last
    println(Console.YELLOW + s"[${fname}: ${sourcecode.Line()}]" + color + s"\n$word" + Console.RESET)
  }

  implicit val debug = false
  def dsay(a: => Any)(implicit filename: sourcecode.File, line: sourcecode.Line, debug: Boolean) = if(debug) say(a)(filename, line)

  def expDecayStep(factor: Float, base: Float, x: Float): Float =
    base + (x - base)*math.exp(-factor).toFloat


  def hardcode[A](a: A)(implicit filename: sourcecode.File, line: sourcecode.Line): A = {
    val fname = filename.value.split("/").last
    println(Console.YELLOW + s"[${fname}: ${sourcecode.Line()}]" + Console.RED + " Warning, using magic hardcoded value" + Console.RESET)
    a
  }
}

case class Params(
  initNodes             : Int    = 10,
  mutationRate          : Double = 0.0,
  connectionMutationMax : Int    = 5,
  paramMutationMax      : Double = 5.0,
)
