package distribution.continuous

import distribution.UnivariateContinuous
import distribution.RandomGeneric
import org.apache.commons.math3.special.Gamma._
import math.{log, exp, pow}

case class InverseGamma(params: (Double,Double)) extends UnivariateContinuous(params) {

  type RvType = Double

  def this(shape:Double, scale:Double) {
    this( (shape, scale) )
  }

  def inSupport(x:RvType) = x > 0

  lazy val (shape, scale) = params
  require(shape > 0 && scale > 0, "InverseGamma(a,b) params invalid.")

  lazy val mean = if (shape > 1) scale / (shape - 1) else Double.PositiveInfinity
  lazy val variance = if (shape > 2) pow(scale / (shape - 1), 2) / (shape - 2) else Double.PositiveInfinity
  lazy val min = 0
  lazy val max = Double.PositiveInfinity
  lazy val mode = scale / (shape + 1)

  override def lpdf(x:RvType):Double = {
    if (inSupport(x)) {
      shape * log(scale) - logGamma(shape) - (shape+1) * log(x) - scale / x
    } else {
      Double.NegativeInfinity
    }
  }

  def pdf(x:RvType):Double = math.exp(lpdf(x))
  def cdf(x:RvType):Double = if (inSupport(x)) {
    regularizedGammaQ(shape, scale / x)
  } else {
    0
  }

  override def toString = {
    s"Gamma(shape:$shape, scale:$scale)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):RvType = {
    rng.nextInverseGamma(shape, scale)
  }
}
