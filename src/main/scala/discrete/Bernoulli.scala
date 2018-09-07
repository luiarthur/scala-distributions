package distribution.discrete

import distribution.Univariate
import distribution.RandomGeneric

case class Bernoulli(params: Double) extends Univariate[Int](params) {
  type RvType = Int

  val p = params
  require(p >= 0 && p <= 1, "In Bernoulli(p): 0 <= p <= 1 required!")

  val mean = p
  val variance = p * (1 - p)

  def inSupport(x:RvType): Boolean = {
    x == 0 || x == 1
  }

  def pdf(x:RvType):Double = x match {
    case 0 => 1 - p
    case 1 => p
    case _ => 0
  }

  def cdf(x:RvType): Double = x match {
    case 0 => 1 - p
    case y if y >= 1 => 1
    case _ => 0
  }


  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextBernoulli(p)
  }
}
