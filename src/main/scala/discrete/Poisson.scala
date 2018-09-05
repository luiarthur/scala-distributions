package distribution.discrete

import distribution.Distribution
import distribution.RandomGeneric
import distribution.SpecialFunctions.logFactorial
import org.apache.commons.math3.special.Gamma.regularizedGammaQ

// TODO: Test
case class Poisson(params: Double) extends Distribution(params) {
  type RvType = Int
  type meanType = Double
  type varType = Double

  val lam = params
  //require(p >= 0 && p <= 1, "In Binomial(n, p): 0 <= p <= 1 required!")
  //require(n >= 0, "Int Binomial(n, p): n >= 0 requried!")

  val mean = lam
  val variance = lam

  override def lpdf(x:Int): Double = {
    x * math.log(lam) - lam - logFactorial(x)
  }

  def pdf(x:Int):Double = {
    math.exp(lpdf(x))
  }

  def cdf(x:Int): Double = {
    // The constant-time method.
    regularizedGammaQ(x + 1, lam)
  }

  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextPoisson(lam)
  }
}
