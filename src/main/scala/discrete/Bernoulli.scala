package distribution.discrete

import distribution.Distribution
import distribution.RandomGeneric

case class Bernoulli(params: Double) extends Distribution(params) {
  type RvType = Int
  type meanType = Double
  type varType = Double

  val p = params
  require(p >= 0 && p <= 1, "In Bernoulli(p): 0 <= p <= 1 required!")

  val mean = p
  val variance = p * (1 - p)

  def pdf(x:Int):Double = x match {
    case 1 => p
    case 0 => 1 - p
    case _ => 0
  }

  def cdf(x:Int):Double = x match {
    case 0 => 1-p
    case y if y >= 1 => 1
    case _ => 0
  }

  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextBernoulli(p)
  }
}
