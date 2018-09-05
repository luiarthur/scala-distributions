package distribution.continuous

import distribution.Distribution
import distribution.RandomGeneric
import org.apache.commons.math3.special.Gamma._
import math.{log, exp, pow}

case class InverseGamma(params: (Double,Double)) extends Distribution(params) {

  type RvType = Double
  type meanType = Double
  type varType = Double

  def this(shape:Double, scale:Double) {
    this( (shape, scale) )
  }

  val (shape, scale) = params
  //require ???

  val mean = if (shape > 1) scale / (shape - 1) else Double.PositiveInfinity
  val variance = if (shape > 2) pow(scale / (shape - 1), 2) / (shape - 2) else Double.PositiveInfinity

  override def lpdf(x:Double):Double = {
    if (x > 0) {
      shape * log(scale) - logGamma(shape) - (shape+1) * log(x) - scale / x
    } else {
      Double.NegativeInfinity
    }
  }

  def pdf(x:Double):Double = math.exp(lpdf(x))
  def cdf(x:Double):Double = if (x > 0) {
    regularizedGammaQ(shape, scale / x)
  } else {
    0
  }

  override def toString = {
    s"Gamma(shape:$shape, scale:$scale)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):Double = {
    rng.nextInverseGamma(shape, scale)
  }
}
