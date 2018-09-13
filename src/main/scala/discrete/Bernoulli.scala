package distribution.discrete

import distribution.UnivariateDiscrete
import distribution.RandomGeneric

case class Bernoulli(p: Double) extends UnivariateDiscrete {
  type RvType = Int

  require(p >= 0 && p <= 1, "In Bernoulli(p): 0 <= p <= 1 required!")

  lazy val mean = p
  lazy val variance = p * (1 - p)
  lazy val min = 0
  lazy val max = 1
  lazy val isMaxInclusive = true
  lazy val isMinInclusive = true
  lazy val mode = if (p <= 0.5) 0 else 1

  def pdf(x:RvType):Double = x match {
    case 0 => 1 - p
    case 1 => p
    case _ => 0
  }

  def cdfInSupport(x:RvType): Double = x match {
    case 0 => 1 - p
    case 1 => 1
  }


  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextBernoulli(p)
  }
}
