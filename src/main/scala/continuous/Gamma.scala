package distribution.continuous

import distribution.UnivariateContinuous
import distribution.RandomGeneric
import org.apache.commons.math3.special.Gamma._
import math.{log, exp}

case class Gamma(params: (Double,Double)) extends UnivariateContinuous(params) {
  type RvType = Double

  def inSupport(x:RvType) = x > 0

  def this(shape:Double, rate:Double) {
    this( (shape, rate) )
  }

  lazy val (shape, rate) = params
  //require(sd > 0, "In Normal(mean, sd): sd > 0 required!")

  lazy val mean = shape / rate
  lazy val variance = mean / rate
  lazy val min = 0
  lazy val max = Double.PositiveInfinity
  lazy val mode = if (shape >= 1) (shape - 1) / rate else 0

  override def lpdf(x:RvType):Double = {
    if (inSupport(x)) {
      shape * log(rate) - logGamma(shape) + (shape - 1) * log(x) - rate * x
    } else {
      Double.NegativeInfinity
    }
  }

  def pdf(x:RvType):Double = math.exp(lpdf(x))
  def cdf(x:RvType):Double = x match {
    case y if inSupport(y) => regularizedGammaP(shape, rate * y)
    case _ => 0
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
