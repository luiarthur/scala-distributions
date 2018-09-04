package distribution.discrete

import distribution.Distribution
import distribution.RandomGeneric
import distribution.SpecialFunctions.logChoose
import org.apache.commons.math3.special.Beta.regularizedBeta

// TODO: Test
case class NegativeBinomial(params: (Int,Double)) extends Distribution(params) {
  type RvType = Int
  type meanType = Double
  type varType = Double

  val (numSuccess, probSuccess) = params
  val mean = numSuccess * (1 - probSuccess) / probSuccess
  val variance = mean / probSuccess
  // require???

  override def lpdf(x:Int): Double = {
    logChoose(numSuccess + x - 1, x) + numSuccess * math.log(probSuccess) + x * math.log(1 - probSuccess)
  }

  def pdf(x:Int):Double = {
    math.exp(lpdf(x))
  }

  override def ccdf(x:Int): Double = {
    regularizedBeta(1 - probSuccess, x + 1, numSuccess)
  }

  def cdf(x:Int): Double = 1 - ccdf(x)

  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextNegativeBinomial(numSuccess, probSuccess)
  }
}
