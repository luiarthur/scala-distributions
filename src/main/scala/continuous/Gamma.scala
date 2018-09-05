package distribution.continuous

import distribution.Distribution
import distribution.RandomGeneric
import org.apache.commons.math3.special.Gamma._
import math.{log, exp}

case class Gamma(params: (Double,Double)) extends Distribution(params) {

  type RvType = Double
  type meanType = Double
  type varType = Double

  def this(shape:Double, rate:Double) {
    this( (shape, rate) )
  }

  val (shape, rate) = params
  //require(sd > 0, "In Normal(mean, sd): sd > 0 required!")

  val mean = shape / rate
  val variance = mean / rate

  override def lpdf(x:Double):Double = {
    if (x > 0) {
      shape * log(rate) - logGamma(shape) + (shape - 1) * log(x) - rate * x
    } else {
      Double.NegativeInfinity
    }
  }

  def pdf(x:Double):Double = math.exp(lpdf(x))
  def cdf(x:Double):Double = if (x > 0) {
    regularizedGammaP(shape, rate * x)
  } else {
    0
  }

  override def toString = {
    s"Gamma(shape:$shape, rate:$rate)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):Double = {
    rng.nextGamma(shape, rate)
  }
}
