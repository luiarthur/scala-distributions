package distribution.discrete

import distribution.Univariate
import distribution.RandomGeneric
import distribution.SpecialFunctions.logFactorial
import org.apache.commons.math3.special.Gamma.regularizedGammaQ

// TODO: Test
case class Poisson(params: Double) extends Univariate(params) {
  type RvType = Int

  val lam = params
  //require(p >= 0 && p <= 1, "In Binomial(n, p): 0 <= p <= 1 required!")
  //require(n >= 0, "Int Binomial(n, p): n >= 0 requried!")

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
