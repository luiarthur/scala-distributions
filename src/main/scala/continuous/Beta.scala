package distribution.continuous

import distribution.Distribution
import distribution.RandomGeneric
import org.apache.commons.math3.special.Gamma._
import org.apache.commons.math3.special.Beta._
import math.{log, exp}

case class Beta(params: (Double,Double)) extends Distribution(params) {

  type RvType = Double
  type meanType = Double
  type varType = Double

  def this(a:Double, b:Double) {
    this( (a, b) )
  }

  val (a, b) = params
  //require(sd > 0, "In Normal(mean, sd): sd > 0 required!")

  val mean = a / (a + b)
  val variance = mean * mean * b / (a * (a + b + 1))

  override def lpdf(x:Double):Double = if (x > 0 && x < 1) {
    logGamma(a + b) - logGamma(a) - logGamma(b) + (a - 1) * log(x) + (b - 1) * log(1 - x) 
  } else {
    Double.NegativeInfinity
  }

  def pdf(x:Double):Double = math.exp(lpdf(x))
  def cdf(x:Double):Double = if (x > 0) {
    regularizedBeta(x, a, b)
  } else {
    0
  }

  override def toString = {
    s"Beta(a:$a, b:$b)"
  }

  def sample[Rng <: distribution.RandomGeneric](rng: Rng):Double = {
    rng.nextBeta(a, b)
  }
}
