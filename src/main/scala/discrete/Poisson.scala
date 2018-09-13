package distribution.discrete

import distribution.UnivariateDiscrete
import distribution.RandomGeneric
import distribution.SpecialFunctions.logFactorial
import org.apache.commons.math3.special.Gamma.regularizedGammaQ

// TODO: Test
case class Poisson(lam: Double) extends UnivariateDiscrete {
  type RvType = Int

  require(lam > 0, "In Poisson(lam): lam > 0 required!")
  
  lazy val min:Double = 0.0
  lazy val max:Double = Double.PositiveInfinity
  lazy val isMaxInclusive = false
  lazy val isMinInclusive = true
  lazy val mode:Int = math.floor(lam).toInt

  val mean = lam
  val variance = lam


  override def lpdf(x:Int): Double = x match {
    case y if inSupport(y) => y * math.log(lam) - lam - logFactorial(y)
    case _ => Double.NegativeInfinity
  }

  def pdf(x:Int):Double = {
    math.exp(lpdf(x))
  }

  def cdfInSupport(x:Int): Double = {
    regularizedGammaQ(x + 1, lam)
  }

  def sample[Rng <: RandomGeneric](rng:Rng): RvType = {
    rng.nextPoisson(lam)
  }
}
