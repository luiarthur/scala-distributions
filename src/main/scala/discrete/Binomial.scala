package distribution.discrete

import distribution.Distribution
import distribution.RandomGeneric
import distribution.SpecialFunctions.{choose, logChoose}

// TODO: Test
case class Binomial(params: (Int,Double)) extends Distribution(params) {
  type RvType = Int
  type meanType = Double
  type varType = Double

  val (n, p) = params
  require(p >= 0 && p <= 1, "In Binomial(n, p): 0 <= p <= 1 required!")
  require(n >= 0, "Int Binomial(n, p): n >= 0 requried!")

  val mean = n * p
  val variance = n * p * (1 - p)

  override def lpdf(x:Int): Double = {
    logChoose(n, x) + x * math.log(p) + (n - x) * math.log(1 - p)
  }

  def pdf(x:Int):Double = {
    math.exp(lpdf(x))
  }

  def cdf(x:Int): Double = {
    def engine(i:Int=x, out:Double=0): Double =  x match {
      case y if y >= n => 1
      case y if y >= 0 => engine(i - 1, pdf(i) + out)
      case _ => out
    }
    
    engine()
  }

  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextBinomial(n, p)
  }
}
