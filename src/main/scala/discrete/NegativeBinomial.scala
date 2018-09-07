package distribution.discrete

import distribution.Univariate
import distribution.RandomGeneric
import distribution.SpecialFunctions.logChoose
import org.apache.commons.math3.special.Beta.regularizedBeta

// TODO: Test
case class NegativeBinomial(params: (Int, Double)) extends NegativeBinomialBase(params)

class NegativeBinomialBase(params: (Int,Double)) extends Univariate[Int](params) {
  type RvType = Int

  val (numSuccess, probSuccess) = params
  val mean = numSuccess * (1 - probSuccess) / probSuccess
  val variance = mean / probSuccess
  require(numSuccess > 0 && probSuccess > 0)

  def inSupport(x:RvType) = x >= 0

  override def lpdf(x:RvType): Double = if (inSupport(x)) {
    logChoose(numSuccess + x - 1, x) + numSuccess * math.log(probSuccess) + x * math.log(1 - probSuccess)
  } else {
    Double.NegativeInfinity
  }

  // Here, x is the number of failures required.
  def pdf(x:RvType):Double = {
    math.exp(lpdf(x))
  }

  override def ccdf(x:RvType): Double = if (inSupport(x)) {
    regularizedBeta(1 - probSuccess, x + 1, numSuccess)
  } else {
    1
  }

  def cdf(x:RvType): Double = 1 - ccdf(x)

  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextNegativeBinomial(numSuccess, probSuccess)
  }
}
