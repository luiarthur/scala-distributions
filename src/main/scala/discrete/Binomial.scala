package distribution.discrete

import distribution.Univariate
import distribution.RandomGeneric
import distribution.SpecialFunctions.logChoose
import org.apache.commons.math3.special.Beta.regularizedBeta

// TODO: Test
case class Binomial(params: (Int,Double)) extends Univariate(params) {
  type RvType = Int

  val (n, p) = params
  //require(p >= 0 && p <= 1, "In Binomial(n, p): 0 <= p <= 1 required!")
  //require(n >= 0, "Int Binomial(n, p): n >= 0 requried!")

  def inSupport(x:RvType) = 0 <= x && x <= n

  val mean = n * p
  val variance = n * p * (1 - p)

  override def lpdf(x:RvType): Double = if (inSupport(x)) {
    logChoose(n, x) + x * math.log(p) + (n - x) * math.log(1 - p)
  } else Double.NegativeInfinity

  def pdf(x:RvType):Double = {
    math.exp(lpdf(x))
  }

  def cdf(x:RvType): Double = x match {
    case y if inSupport(y) => regularizedBeta(1 - p, n - x, x + 1)
    case y if y > n => 1
    case _ => 0
  }

  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextBinomial(n, p)
  }
}
