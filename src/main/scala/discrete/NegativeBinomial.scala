package distribution.discrete

import distribution.UnivariateDiscrete
import distribution.RandomGeneric
import distribution.SpecialFunctions.logChoose
import org.apache.commons.math3.special.Beta.regularizedBeta

// TODO: Test
case class NegativeBinomial(numSuccess: Int, probSuccess:Double) extends NegativeBinomialBase(numSuccess, probSuccess)

class NegativeBinomialBase(numSuccess: Int, probSuccess:Double) extends UnivariateDiscrete {
  require(numSuccess > 0 && probSuccess > 0)

  type RvType = Int

  lazy val mean = numSuccess * (1 - probSuccess) / probSuccess
  lazy val variance = mean / probSuccess
  lazy val min = 0
  lazy val max = Double.PositiveInfinity
  lazy val isMaxInclusive = false
  lazy val isMinInclusive = true

  lazy val mode = if (numSuccess > 1) {
    math.floor((numSuccess - 1) * (1 - probSuccess) / probSuccess).toInt
  } else 0


  override def lpdf(x:RvType): Double = if (inSupport(x)) {
    logChoose(numSuccess + x - 1, x) + numSuccess * math.log(probSuccess) + x * math.log(1 - probSuccess)
  } else {
    Double.NegativeInfinity
  }

  // Here, x is the number of failures required.
  def pdf(x:RvType):Double = {
    math.exp(lpdf(x))
  }

  def cdfInSupport(x:RvType): Double = {
    1 - regularizedBeta(1 - probSuccess, x + 1, numSuccess)
  }

  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextNegativeBinomial(numSuccess, probSuccess)
  }
}
