package distribution.discrete

import distribution.Univariate
import distribution.RandomGeneric
import distribution.SpecialFunctions.logFactorial
import org.apache.commons.math3.special.Gamma.regularizedGammaQ

// TODO: Test
case class Poisson(params: Double) extends Univariate[Int](params) {
  type RvType = Int

  val lam = params
  require(lam > 0, "In Poisson(lam): lam > 0 required!")

  val mean = lam
  val variance = lam

  def inSupport(x:RvType) = x >= 0

  override def lpdf(x:Int): Double = x match {
    case y if inSupport(y) => y * math.log(lam) - lam - logFactorial(y)
    case _ => Double.NegativeInfinity
  }

  def pdf(x:Int):Double = {
    math.exp(lpdf(x))
  }

  def cdf(x:Int): Double = x match {
    case y if inSupport(y) => regularizedGammaQ(y + 1, lam)
    case _ => 0
  }

  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextPoisson(lam)
  }
}
