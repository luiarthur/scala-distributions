package distribution.continuous

import distribution.UnivariateContinuous
import distribution.RandomGeneric
import org.apache.commons.math3.special.Gamma._
import math.{log, exp}

case class Gamma(shape:Double, rate:Double) extends UnivariateContinuous {
  type RvType = Double

  require(shape > 0 && rate > 0, "Gamma(shape, rate): shape, rate > 0 required!")

  lazy val mean = shape / rate
  lazy val variance = mean / rate
  lazy val min = 0
  lazy val max = Double.PositiveInfinity
  lazy val mode = if (shape >= 1) (shape - 1) / rate else 0
  lazy val isMaxInclusive = false
  lazy val isMinInclusive = false

  override def lpdf(x:RvType):Double = {
    if (inSupport(x)) {
      shape * log(rate) - logGamma(shape) + (shape - 1) * log(x) - rate * x
    } else {
      Double.NegativeInfinity
    }
  }

  def pdf(x:RvType):Double = math.exp(lpdf(x))
  def cdfInSupport(x:RvType):Double =  {
    regularizedGammaP(shape, rate * x)
  }

  override def toString = {
    s"Gamma(shape:$shape, rate:$rate)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):RvType = {
    rng.nextGamma(shape, rate)
  }

  override def quantile(p: Double, eps:Double=1E-12, maxIter:Int=10000, verbose:Int=1): Double = {
    require(0 <= p && p <= 1, "quantile(p, eps): 0 <= p <= 1 required!")
    if (shape >= 1) quantileNewton(p, mode, eps, maxIter, verbose) else {
      // TODO: First log transform, then find quantile, then exponentiate quantile.
      quantileNewton(p, mean, eps, maxIter, verbose)
    }
  }
}
